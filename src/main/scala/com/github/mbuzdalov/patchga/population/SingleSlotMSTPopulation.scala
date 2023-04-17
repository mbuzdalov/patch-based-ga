package com.github.mbuzdalov.patchga.population

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import com.github.mbuzdalov.patchga.config.*
import com.github.mbuzdalov.patchga.util.Loops

trait SingleSlotMSTPopulation extends Population:
  self: IndividualType & FitnessType & PatchType & MaximumPatchSize & NewRandomIndividual & SimpleFitnessFunction & IncrementalFitnessFunction =>

  private class Edge private(source: Node, fwdPatch: ImmutablePatch, bwdPatch: ImmutablePatch, val target: Node, reverseOrNull: Edge | Null):
    def this(source: Node, fwdPatch: ImmutablePatch, bwdPatch: ImmutablePatch, target: Node) =
      this(source, fwdPatch, bwdPatch, target, null)
    def patch: ImmutablePatch = fwdPatch
    val reverse: Edge = if reverseOrNull != null then reverseOrNull else Edge(target, bwdPatch, fwdPatch, source, this)

  class Node(val fitness: Fitness):
    private[SingleSlotMSTPopulation] var referenceCount = 1
    private[SingleSlotMSTPopulation] val edges = new ArrayBuffer[Edge](2)

  override type IndividualHandle = Node

  private var masterIndividual = newRandomIndividual()
  private val masterPatch = createMutablePatch()
  private var currentNode: Node = _
  private var sumPatchSizes: Long = 0

  def totalSizeOfPatches: Long = sumPatchSizes

  override def fitnessH(handle: IndividualHandle): Fitness = handle.fitness

  override def discardH(handle: IndividualHandle): Unit =
    handle.referenceCount -= 1
    if handle.referenceCount == 0 then
      disconnectRecursively(handle)

  override def newRandomIndividualH(): Node =
    if currentNode == null then
      // This happens only when we are requested for the first time
      currentNode = Node(computeFitness(masterIndividual))
    else
      // Otherwise, there is already an existing tree, so we have to add the new individual somehow
      val tempIndividual = newRandomIndividual()
      val newNode = Node(computeFitness(tempIndividual))
      initializeMutablePatchFromTwoIndividuals(masterPatch, tempIndividual, masterIndividual)
      masterIndividual = tempIndividual
      reconnect(newNode)
    end if
    currentNode

  override def mutateH(handle: Node, distance: Int): Node =
    assert(handle.referenceCount > 0)
    rewindToGivenNode(null, handle)
    currentNode = handle
    // This is the reverse patch: that is, from the newly generated node to currentNode
    initializeMutablePatchFromDistance(masterPatch, distance)
    val newToOldPatch = createImmutableVersion(masterPatch)
    val oldToNewPatch = reversedImmutablePatch(newToOldPatch)
    val newNode = Node(computeFitnessFunctionIncrementally(masterIndividual, currentNode.fitness, oldToNewPatch))
    reconnect(newNode)
    currentNode

  override def crossoverH(mainParent: Node, auxParent: Node, inDifferingBits: Int => Int, inSameBits: Int => Int): Node =
    assert(mainParent.referenceCount > 0)
    assert(auxParent.referenceCount > 0)
    rewindToGivenNode(null, mainParent)
    currentNode = mainParent
    clearMutablePatch(masterPatch)
    collectPatchFromCurrent(null, auxParent)
    val interParentDistance = mutablePatchSize(masterPatch)
    val desiredInDifferent = inDifferingBits(interParentDistance)
    val desiredInSame = inSameBits(maximumPatchSize - interParentDistance) // very brittle!
    applyCrossoverRequest(masterPatch, desiredInDifferent, desiredInSame)
    val oldToNewPatch = createImmutableVersion(masterPatch)
    val newNode = Node(computeFitnessFunctionIncrementally(masterIndividual, currentNode.fitness, oldToNewPatch))
    reconnect(newNode)
    currentNode

  private def reconnect(newNode: Node): Unit =
    val shortestDistance = computeShortestDistance(null, currentNode)
    rewindCurrentNodeToDistance(null, currentNode, shortestDistance)
    assert(mutablePatchSize(masterPatch) == shortestDistance)
    val newToBestPatch = createImmutableVersion(masterPatch)
    val bestToNewPatch = reversedImmutablePatch(newToBestPatch)
    val newToBestEdge = Edge(newNode, newToBestPatch, bestToNewPatch, currentNode)
    newNode.edges.addOne(newToBestEdge)
    currentNode.edges.addOne(newToBestEdge.reverse)
    currentNode = newNode
    sumPatchSizes += shortestDistance

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

  private def computeShortestDistance(parentNode: Node, currNode: Node): Int =
    val edges = currNode.edges
    var result = if currNode.referenceCount > 0 then mutablePatchSize(masterPatch) else Int.MaxValue
    Loops.loop(0, edges.size) { i =>
      val edge = edges(i)
      if edge.target != parentNode then
        addToMutablePatch(masterPatch, edge.patch)
        result = math.min(result, computeShortestDistance(currNode, edge.target))
        addToMutablePatch(masterPatch, edge.reverse.patch)
    }
    result

  private def rewindCurrentNodeToDistance(parentNode: Node, currNode: Node, distance: Int): Boolean =
    if currNode.referenceCount > 0 && mutablePatchSize(masterPatch) == distance then
      currentNode = currNode
      true
    else
      val edges = currNode.edges
      Loops.exists(0, edges.size) { i =>
        val edge = edges(i)
        if edge.target == parentNode then
          false
        else
          addToMutablePatch(masterPatch, edge.patch)
          if rewindCurrentNodeToDistance(currNode, edge.target, distance) then
            true
          else
            addToMutablePatch(masterPatch, edge.reverse.patch)
            false
      }

  private def rewindToGivenNode(parentNode: Node, currNode: Node): Boolean =
    if currNode == currentNode then
      true
    else
      val edges = currNode.edges
      Loops.exists(0, edges.size) { i =>
        val edge = edges(i)
        if edge.target == parentNode then
          false
        else
          if rewindToGivenNode(currNode, edge.target) then
            applyToIndividual(masterIndividual, edge.reverse.patch)
            true
          else
            false
      }

  private def collectPatchFromCurrent(parentNode: Node, currNode: Node): Boolean =
    if currNode == currentNode then
      true
    else
      val edges = currNode.edges
      Loops.exists(0, edges.size) { i =>
        val edge = edges(i)
        if edge.target == parentNode then
          false
        else
          if collectPatchFromCurrent(currNode, edge.target) then
            addToMutablePatch(masterPatch, edge.patch)
            true
          else
            false
      }
