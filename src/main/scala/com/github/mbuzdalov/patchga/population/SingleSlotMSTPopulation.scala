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
    private val edges = new ArrayBuffer[Edge](2)
    private var nextEdgeInPath = -1

    private[SingleSlotMSTPopulation] def addEdge(edge: Edge): Unit =
      edges.addOne(edge)
    private[SingleSlotMSTPopulation] def fitness: Fitness
    private[SingleSlotMSTPopulation] def hasEdgeInPath: Boolean = nextEdgeInPath >= 0
    private[SingleSlotMSTPopulation] def getEdgeInPath: Edge = edges(nextEdgeInPath)
    private[SingleSlotMSTPopulation] def clearEdgeInPath(): Unit = nextEdgeInPath = -1
    private[SingleSlotMSTPopulation] inline def iterateOverEdges(parent: Node)(inline fun: Edge => Unit): Unit =
      Loops.loop(0, edges.size) { i =>
        val edge = edges(i)
        if edge.target != parent then fun(edge)
      }

    private[SingleSlotMSTPopulation] inline def findFirstEdgeForPath(parent: Node)(inline fun: Edge => Boolean): Boolean =
      nextEdgeInPath = Loops.find(0, edges.size) { i =>
        val edge = edges(i)
        edge.target != parent && fun(edge)
      }
      if nextEdgeInPath == edges.size then nextEdgeInPath = -1
      nextEdgeInPath >= 0

    private[SingleSlotMSTPopulation] inline def useLastEdgeForPath(parent: Node)(inline fun: Edge => Boolean): Unit =
      nextEdgeInPath = -1
      Loops.loop(0, edges.size) { i =>
        val edge = edges(i)
        if edge.target != parent && fun(edge) then nextEdgeInPath = i
      }

    @tailrec
    private[SingleSlotMSTPopulation] final def tryDisconnect(): Unit =
      if edges.size == 1 then
        val theEdge = edges(0)
        sumPatchSizes -= immutablePatchSize(theEdge.patch)
        edges.clear()
        val otherNode = theEdge.target
        if this == currentNode then
          applyToIndividual(masterIndividual, theEdge.patch)
          currentNode = otherNode
        val index = otherNode.edges.indexOf(theEdge.reverse)
        assert(index >= 0)
        otherNode.edges.remove(index)
        if otherNode.referenceCount == 0 then
          otherNode.tryDisconnect()


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
      handle.tryDisconnect()

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
      currentNode.addEdge(bestToNewEdge)
      newNode.addEdge(bestToNewEdge.reverse)
      sumPatchSizes += shortestDistance
      currentNode = newNode
      currentNode

  private def buildReversePathToShortestDistance(parent: Node, curr: Node): Int =
    var currentDistance = mutablePatchSize(masterPatch)
    curr.useLastEdgeForPath(parent) { edge =>
      prependToMutablePatch(masterPatch, edge.reverse.patch)
      val result = buildReversePathToShortestDistance(curr, edge.target)
      prependToMutablePatch(masterPatch, edge.patch)
      val oldDistance = currentDistance
      if currentDistance > result then currentDistance = result
      oldDistance > currentDistance
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
    curr.iterateOverEdges(parent) { edge =>
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
    curr.iterateOverEdges(parent) { edge =>
      appendToMutablePatch(masterPatch, edge.patch)
      collectHandlesAtDistanceImpl(curr, edge.target, distance, buffer)
      appendToMutablePatch(masterPatch, edge.reverse.patch)
    }

  private def buildPathToNode(parent: Node, curr: Node, target: Node): Boolean =
    if curr == target then
      curr.clearEdgeInPath()
      true
    else curr.findFirstEdgeForPath(parent)(edge => buildPathToNode(curr, edge.target, target))

  @tailrec
  private def appendForwardPathToMasterPatch(node: Node): Unit =
    if node.hasEdgeInPath then
      val theEdge = node.getEdgeInPath
      appendToMutablePatch(masterPatch, theEdge.patch)
      appendForwardPathToMasterPatch(theEdge.target)

  private def rewindMastersByBackwardPath(): Unit =
    while currentNode.hasEdgeInPath do
      val theEdge = currentNode.getEdgeInPath
      prependToMutablePatch(masterPatch, theEdge.reverse.patch)
      applyToIndividual(masterIndividual, theEdge.reverse.patch)
      currentNode = theEdge.target

  private def rewindMasterIndividualByPath(): Unit =
    while currentNode.hasEdgeInPath do
      val theEdge = currentNode.getEdgeInPath
      applyToIndividual(masterIndividual, theEdge.patch)
      currentNode = theEdge.target
