package com.github.mbuzdalov.patchga.population

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.util.Loops

trait SingleSlotMSTPopulation(allowDuplicates: Boolean) extends Population:
  self: IndividualType & FitnessType & PatchType & MaximumPatchSize & NewRandomIndividual 
    & SimpleFitnessFunction & IncrementalFitnessFunction & RandomProvider =>

  private class Edge private(source: Node, fwdPatch: ImmutablePatch, bwdPatch: ImmutablePatch, val target: Node, reverseOrNull: Edge | Null):
    def this(source: Node, fwdPatch: ImmutablePatch, bwdPatch: ImmutablePatch, target: Node) =
      this(source, fwdPatch, bwdPatch, target, null)
    def patch: ImmutablePatch = fwdPatch
    val reverse: Edge = if reverseOrNull != null then reverseOrNull else Edge(target, bwdPatch, fwdPatch, source, this)

  abstract class Node:
    private[SingleSlotMSTPopulation] var referenceCount = 1
    private[SingleSlotMSTPopulation] val edges = new ArrayBuffer[Edge](2)
    private[SingleSlotMSTPopulation] var nextEdgeInPath = -1
    def fitness: Fitness

  private class KnownFitnessNode(val fitness: Fitness) extends Node

  override type IndividualHandle = Node

  private val masterIndividual = newRandomIndividual()
  private val masterPatch = createMutablePatch()
  private var currentNode: Node = _
  private var sumPatchSizes: Long = 0

  def totalSizeOfPatches: Long = sumPatchSizes

  override def fitnessH(handle: IndividualHandle): Fitness = handle.fitness

  override def discardH(handle: IndividualHandle): Unit =
    handle.referenceCount -= 1
    if handle.referenceCount == 0 then
      disconnectRecursively(handle)

  override def newRandomIndividualH(): IndividualHandle =
    if currentNode == null then
      // This happens only when we are requested for the first time
      currentNode = KnownFitnessNode(computeFitness(masterIndividual))
      currentNode
    else
      // Otherwise, there is already an existing tree, so we have to add the new individual somehow
      // We basically simulate mutation from the current node.
      // Current impl of binomial distribution is somewhat too slow for Bin(n, 0.5), so we do the naive way.
      var distance = 0
      Loops.loop(0, maximumPatchSize)(_ => if random.nextBoolean() then distance += 1)
      mutateH(currentNode, distance)
    end if

  override def mutateH(handle: IndividualHandle, distance: Int): IndividualHandle =
    assert(handle.referenceCount > 0)
    buildPathToNode(null, currentNode, handle)
    rewindMasterIndividualByPath()
    initializeMutablePatchFromDistance(masterPatch, distance)
    newNodeFromPatch()

  override def crossoverH(mainParent: IndividualHandle, auxParent: IndividualHandle,
                          inDifferingBits: Int => Int, inSameBits: Int => Int): IndividualHandle =
    assert(mainParent.referenceCount > 0)
    assert(auxParent.referenceCount > 0)
    buildPathToNode(null, currentNode, mainParent)
    rewindMasterIndividualByPath()
    buildPathToNode(null, mainParent, auxParent)
    clearMutablePatch(masterPatch)
    appendForwardPathToMasterPatch(mainParent)
    val interParentDistance = mutablePatchSize(masterPatch)
    val desiredInDifferent = inDifferingBits(interParentDistance)
    val desiredInSame = inSameBits(maximumPatchSize - interParentDistance) // very brittle!
    applyCrossoverRequest(masterPatch, desiredInDifferent, desiredInSame)
    newNodeFromPatch()

  private def newNodeFromPatch(): IndividualHandle =
    // there is currentNode, and masterPatch points _from_ it _to_ the new node
    val shortestDistance = buildReversePathToShortestDistance(null, currentNode)
    // the path from currentNode now goes backwards to the node where we need to move
    rewindMastersByBackwardPath()
    if shortestDistance == 0 && !allowDuplicates then
      currentNode.referenceCount += 1
      currentNode
    else
      // now currentNode is our new immediate parent, and masterPatch points exactly to the new node to create
      val bestToNewPatch = createImmutableVersion(masterPatch)
      val newToBestPatch = reversedImmutablePatch(bestToNewPatch)
      val newNode = KnownFitnessNode(computeFitnessFunctionIncrementally(masterIndividual, currentNode.fitness, bestToNewPatch))
      clearMutablePatch(masterPatch)
      // now masterIndividual matches newNode
      val bestToNewEdge = Edge(currentNode, bestToNewPatch, newToBestPatch, newNode)
      currentNode.edges.addOne(bestToNewEdge)
      newNode.edges.addOne(bestToNewEdge.reverse)
      sumPatchSizes += shortestDistance
      currentNode = newNode
      currentNode

  @tailrec
  private def disconnectRecursively(handle: IndividualHandle): Unit =
    if handle.edges.size == 1 then
      val theEdge = handle.edges(0)
      sumPatchSizes -= immutablePatchSize(theEdge.patch)
      handle.edges.clear()
      val otherNode = theEdge.target
      if handle == currentNode then
        applyToIndividual(masterIndividual, theEdge.patch)
        currentNode = otherNode
      val index = otherNode.edges.indexOf(theEdge.reverse)
      assert(index >= 0)
      otherNode.edges.remove(index)
      if otherNode.referenceCount == 0 then
        disconnectRecursively(otherNode)

  private def buildReversePathToShortestDistance(parent: Node, curr: Node): Int =
    var currentDistance = mutablePatchSize(masterPatch)
    curr.nextEdgeInPath = -1
    val edges = curr.edges
    Loops.loop(0, edges.size) { i =>
      val edge = edges(i)
      if edge.target != parent then // could shortcut if currentDistance is small, but listeners will starve
        prependToMutablePatch(masterPatch, edge.reverse.patch)
        val result = buildReversePathToShortestDistance(curr, edge.target)
        prependToMutablePatch(masterPatch, edge.patch)
        if currentDistance > result then
          currentDistance = result
          curr.nextEdgeInPath = i
    }
    currentDistance

  def collectDistanceToHandles(base: IndividualHandle, consumer: (IndividualHandle, Int) => Unit): Unit =
    assert(base.referenceCount > 0)
    buildPathToNode(null, currentNode, base)
    rewindMasterIndividualByPath()
    clearMutablePatch(masterPatch)
    collectDistanceToHandlesImpl(null, currentNode, consumer)

  private def collectDistanceToHandlesImpl(parent: Node, curr: Node, function: (IndividualHandle, Int) => Unit): Unit =
    if curr.referenceCount > 0 then function(curr, mutablePatchSize(masterPatch))
    val edges = curr.edges
    Loops.loop(0, edges.size) { i =>
      val edge = edges(i)
      if edge.target != parent then
        appendToMutablePatch(masterPatch, edge.patch)
        collectDistanceToHandlesImpl(curr, edge.target, function)
        appendToMutablePatch(masterPatch, edge.reverse.patch)
    }

  def collectHandlesAtDistance(base: IndividualHandle, distance: Int, buffer: ArrayBuffer[IndividualHandle]): Unit =
    assert(base.referenceCount > 0)
    buildPathToNode(null, currentNode, base)
    rewindMasterIndividualByPath()
    clearMutablePatch(masterPatch)
    buffer.clear()
    collectHandlesAtDistanceImpl(null, currentNode, distance, buffer)

  private def collectHandlesAtDistanceImpl(parent: Node, curr: Node, distance: Int, buffer: ArrayBuffer[Node]): Unit =
    if curr.referenceCount > 0 && mutablePatchSize(masterPatch) == distance then buffer.addOne(curr)
    val edges = curr.edges
    Loops.loop(0, edges.size) { i =>
      val edge = edges(i)
      if edge.target != parent then
        appendToMutablePatch(masterPatch, edge.patch)
        collectHandlesAtDistanceImpl(curr, edge.target, distance, buffer)
        appendToMutablePatch(masterPatch, edge.reverse.patch)
    }

  private def buildPathToNode(parent: Node, curr: Node, target: Node): Boolean =
    if curr == target then
      curr.nextEdgeInPath = -1
      true
    else
      val edges = curr.edges
      Loops.exists(0, edges.size) { i =>
        val edge = edges(i)
        if edge.target != parent && buildPathToNode(curr, edge.target, target) then
          curr.nextEdgeInPath = i
          true
        else
          false
      }

  @tailrec
  private def appendForwardPathToMasterPatch(node: Node): Unit =
    if node.nextEdgeInPath >= 0 then
      val theEdge = node.edges(node.nextEdgeInPath)
      appendToMutablePatch(masterPatch, theEdge.patch)
      appendForwardPathToMasterPatch(theEdge.target)

  private def rewindMastersByBackwardPath(): Unit =
    while currentNode.nextEdgeInPath >= 0 do
      val theEdge = currentNode.edges(currentNode.nextEdgeInPath)
      prependToMutablePatch(masterPatch, theEdge.reverse.patch)
      applyToIndividual(masterIndividual, theEdge.reverse.patch)
      currentNode = theEdge.target

  private def rewindMasterIndividualByPath(): Unit =
    while currentNode.nextEdgeInPath >= 0 do
      val theEdge = currentNode.edges(currentNode.nextEdgeInPath)
      applyToIndividual(masterIndividual, theEdge.patch)
      currentNode = theEdge.target
