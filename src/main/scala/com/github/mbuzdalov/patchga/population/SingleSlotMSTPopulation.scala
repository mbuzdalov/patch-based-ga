package com.github.mbuzdalov.patchga.population

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.util.Loops

trait SingleSlotMSTPopulation extends Population:
  self: IndividualType & FitnessType & PatchType & MaximumPatchSize & NewRandomIndividual & SimpleFitnessFunction & IncrementalFitnessFunction & RandomProvider =>

  private class Edge private(source: Node, fwdPatch: ImmutablePatch, bwdPatch: ImmutablePatch, val target: Node, reverseOrNull: Edge | Null):
    def this(source: Node, fwdPatch: ImmutablePatch, bwdPatch: ImmutablePatch, target: Node) =
      this(source, fwdPatch, bwdPatch, target, null)
    def patch: ImmutablePatch = fwdPatch
    val reverse: Edge = if reverseOrNull != null then reverseOrNull else Edge(target, bwdPatch, fwdPatch, source, this)

  class Node(val fitness: Fitness, val sequentialIndex: Long):
    private[SingleSlotMSTPopulation] var referenceCount = 1
    private[SingleSlotMSTPopulation] val edges = new ArrayBuffer[Edge](2)
    private[SingleSlotMSTPopulation] var nextEdgeInPath = -1

  trait NodePairListener:
    def accept(n1: Node, n2: Node, distance: Int): Unit

  override type IndividualHandle = Node

  private val masterIndividual = newRandomIndividual()
  private val masterPatch = createMutablePatch()
  private var currentNode: Node = _
  private var sumPatchSizes: Long = 0
  private var numberOfNodesGenerated: Long = 0
  private val listeners = new ArrayBuffer[NodePairListener]()

  private def nextNewNode(fitness: Fitness): Node =
    val result = Node(fitness, numberOfNodesGenerated)
    numberOfNodesGenerated += 1
    result

  def addNodePairListener(listener: NodePairListener): Unit = listeners.addOne(listener)

  def totalSizeOfPatches: Long = sumPatchSizes

  override def fitnessH(handle: IndividualHandle): Fitness = handle.fitness

  override def discardH(handle: IndividualHandle): Unit =
    handle.referenceCount -= 1
    if handle.referenceCount == 0 then
      disconnectRecursively(handle)

  override def newRandomIndividualH(): IndividualHandle =
    if currentNode == null then
      // This happens only when we are requested for the first time
      currentNode = nextNewNode(computeFitness(masterIndividual))
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
    // now currentNode is our new immediate parent, and masterPatch points exactly to the new node to create
    val bestToNewPatch = createImmutableVersion(masterPatch)
    val newToBestPatch = reversedImmutablePatch(bestToNewPatch)
    val newNode = nextNewNode(computeFitnessFunctionIncrementally(masterIndividual, currentNode.fitness, bestToNewPatch))
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

  private def notifyListenersReturnDistance(curr: Node, distance: Int): Int =
    listeners.foreach(_.accept(currentNode, curr, distance))
    distance

  private def buildReversePathToShortestDistance(parent: Node, curr: Node): Int =
    var currentDistance = if curr.referenceCount > 0 then
      notifyListenersReturnDistance(curr, mutablePatchSize(masterPatch))
    else Int.MaxValue
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
