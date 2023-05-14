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
    def length: Int = immutablePatchSize(patch)
    val reverse: Edge = if reverseOrNull != null then reverseOrNull else Edge(target, bwdPatch, fwdPatch, source, this)
    def addMe(): Unit =
      sumPatchSizes += length
      target.addEdge(reverse)
      reverse.target.addEdge(this)

  abstract class Node:
    private[SingleSlotMSTPopulation] var referenceCount = 1
    private val edges = new ArrayBuffer[Edge](2)
    private var nextEdgeInPath: Edge = _

    def getReferenceCount: Int = referenceCount
    
    private[SingleSlotMSTPopulation] def nEdges: Int = edges.size
    private[SingleSlotMSTPopulation] def addEdge(edge: Edge): Unit =
      edges.addOne(edge)
    private[SingleSlotMSTPopulation] def removeEdge(edge: Edge): Unit =
      val index = edges.indexOf(edge)
      assert(index >= 0)
      edges.remove(index)
    private[SingleSlotMSTPopulation] def fitness: Fitness
    private[SingleSlotMSTPopulation] def hasEdgeInPath: Boolean = nextEdgeInPath != null
    private[SingleSlotMSTPopulation] def getEdgeInPath: Edge = nextEdgeInPath
    private[SingleSlotMSTPopulation] def clearEdgeInPath(): Unit = nextEdgeInPath = null
    private[SingleSlotMSTPopulation] inline def iterateOverEdges(parent: Node)(inline fun: Edge => Unit): Unit =
      Loops.loop(0, edges.size) { i =>
        val edge = edges(i)
        if edge.target != parent then fun(edge)
      }

    private[SingleSlotMSTPopulation] inline def iterateOverOldEdges(parent: Node)(inline fun: Edge => Boolean): Unit =
      nextEdgeInPath = null
      val oldEdges = edges.toArray
      edges.clear()
      Loops.loop(0, oldEdges.length) { i =>
        val edge = oldEdges(i)
        if edge.target != parent then
          sumPatchSizes -= edge.length
          if fun(edge) then nextEdgeInPath = edge
      }

    private[SingleSlotMSTPopulation] inline def findFirstEdgeForPath(parent: Node)(inline fun: Edge => Boolean): Boolean =
      val nextEdgeInPathIndex = Loops.find(0, edges.size) { i =>
        val edge = edges(i)
        edge.target != parent && fun(edge)
      }
      nextEdgeInPath = if nextEdgeInPathIndex == edges.size then null else edges(nextEdgeInPathIndex)
      nextEdgeInPath != null

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
        otherNode.removeEdge(theEdge.reverse)
        if otherNode.referenceCount == 0 then
          otherNode.tryDisconnect()


  private class KnownFitnessNode(val fitness: Fitness) extends Node

  private class LateFitnessEvaluationNode extends Node:
    private var shortestEdge: Edge = _
    private var computedFitness: Fitness = _
    override private[SingleSlotMSTPopulation] def addEdge(edge: Edge): Unit =
      super.addEdge(edge)
      if shortestEdge == null || shortestEdge.length >= edge.length then
        shortestEdge = edge

    override private[SingleSlotMSTPopulation] def fitness: Fitness = computedFitness
    private[SingleSlotMSTPopulation] def computeFitnessAndSelectResult(): Node =
      val parent = shortestEdge.target
      if shortestEdge.length == 0 then
        assert(nEdges == 1)
        referenceCount = 0
        parent.referenceCount += 1
        tryDisconnect()
        if allowDuplicates then
          buildPathToNode(null, currentNode, parent)
          rewindMasterIndividualByPath()
          computedFitness = computeFitnessFunctionIncrementally(masterIndividual, parent.fitness, shortestEdge.reverse.patch)
        parent
      else
        buildPathToNode(null, currentNode, parent)
        rewindMasterIndividualByPath()
        currentNode = this
        computedFitness = computeFitnessFunctionIncrementally(masterIndividual, parent.fitness, shortestEdge.reverse.patch)
        this

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
    buildPathToNode(null, currentNode, auxParent)
    clearMutablePatch(masterPatch)
    appendForwardPathToMasterPatch(currentNode)
    val interParentDistance = mutablePatchSize(masterPatch)
    val desiredInDifferent = inDifferingBits(interParentDistance)
    val desiredInSame = inSameBits(maximumPatchSize - interParentDistance) // very brittle!
    applyCrossoverRequest(masterPatch, desiredInDifferent, desiredInSame)
    newNodeFromPatch()

  private def newNodeFromPatch(): IndividualHandle =
    val newNode = new LateFitnessEvaluationNode()
    rebuildMSTOnInsertion(null, currentNode, newNode) match
      case e: Edge => e.addMe()
      case i: Int => addEdgeAtTheEndOfChain(currentNode, i, newNode)
    newNode.computeFitnessAndSelectResult()

  private def addEdgeAtTheEndOfChain(node: Node, expectedLength: Int, target: Node): Unit =
    if node.hasEdgeInPath then
      val edge = node.getEdgeInPath
      prependToMutablePatch(masterPatch, edge.reverse.patch)
      addEdgeAtTheEndOfChain(edge.target, expectedLength, target)
      prependToMutablePatch(masterPatch, edge.patch)
    else
      val fwd = createImmutableVersion(masterPatch)
      val bwd = reversedImmutablePatch(fwd)
      Edge(node, fwd, bwd, target).addMe()

  private def rebuildMSTOnInsertion(parent: Node, curr: Node, inserted: Node): Edge | Int =
    // A normal edge is stored as `Edge`
    // A request to add an uplink edge at the end of the nextEdgeInPath chain is stored as `Int` meaning its length.
    var myPendingEdge: Edge | Int = mutablePatchSize(masterPatch)
    var aliveChildren = 0
    // Loop over all children
    curr.iterateOverOldEdges(parent) { edge =>
      // Call oneself recursively
      prependToMutablePatch(masterPatch, edge.reverse.patch)
      val childPendingEdge = rebuildMSTOnInsertion(curr, edge.target, inserted)
      prependToMutablePatch(masterPatch, edge.patch)

      val pendingLength = myPendingEdge match
        case e: Edge => e.length
        case i: Int => i

      aliveChildren += 1 // we'll subtract one for the dead one
      childPendingEdge match
        case chEdge: Edge =>
          if chEdge.length < edge.length then
            // Child's missing edge is better than the edge to child, and is a real edge, so we add the former back
            chEdge.addMe()
            // If the edge to child is not worse than pending, replace the pending to get rid of potentially virtual myPendingEdge
            if edge.length <= pendingLength then myPendingEdge = edge
          else
            // The edge to child is better than the child's missing edge, so we add the former back
            edge.addMe()
            // If the child's missing edge is not worse than pending, replace the pending, as the child's one is real
            if chEdge.length <= pendingLength then myPendingEdge = chEdge
          false
        case chInt: Int =>
          if chInt == -1 then
            // -1 means the target is dead
            aliveChildren -= 1
            false
          else
            // Strict less-than here, because if they are equal, we prefer to avoid add virtual edges
            if chInt < edge.length then
              // Have to add the child's missing edge, which is virtual.
              prependToMutablePatch(masterPatch, edge.reverse.patch)
              addEdgeAtTheEndOfChain(edge.target, chInt, inserted)
              prependToMutablePatch(masterPatch, edge.patch)
              // If the edge to child is not worse than pending, replace the pending to get rid of potentially virtual myPendingEdge
              if edge.length <= pendingLength then myPendingEdge = edge
              false
            else
              edge.addMe()
              if chInt < pendingLength then
                // Child's missing edge is virtual and is better than myPendingEdge
                myPendingEdge = chInt
                // Retuning true means we point to that child, which will allow to route addEdgeAtTheEndOfChain where needed
                true
              else
                false
    }
    if aliveChildren == 0 && curr.referenceCount == 0 then -1 else myPendingEdge

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

  private def rewindMasterIndividualByPath(): Unit =
    while currentNode.hasEdgeInPath do
      val theEdge = currentNode.getEdgeInPath
      applyToIndividual(masterIndividual, theEdge.patch)
      currentNode = theEdge.target
