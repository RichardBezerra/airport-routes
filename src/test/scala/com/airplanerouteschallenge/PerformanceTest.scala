package com.airplanerouteschallenge

import com.airplanerouteschallenge.ExampleRoutes.providedRoutes
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.util.Success

class PerformanceTest extends AnyFlatSpec with Matchers {

  // Warming up JVM
  TopologicalOrderingFinder
    .createTopologicalOrder(TopologicalOrderingFinder
      .buildGraph(ExampleRoutes.providedRoutes), ExampleRoutes.providedRoutes)

  DirectedCycleGraphFinder.findShortestPath(providedRoutes, Airport("DUB"), Airport("SYD"))

  val mockFinder: ShortestPathFinder = (_: Seq[Route], _: Airport, _: Airport) => Success(Nil)

  val graphShapeBaseLine: (Int, Int) = (mockFinder.airports(ExampleRoutes.providedRoutes).size,
    ExampleRoutes.providedRoutes.size)

  val plus10Times: Seq[Route] = plus(ExampleRoutes.providedRoutes, 10)

  val graphShapePlus10Times: (Int, Int) = (mockFinder.airports(plus10Times).size, plus10Times.size)

  val plus100Times: Seq[Route] = plus(ExampleRoutes.providedRoutes, 100)

  val graphShapePlus100Times: (Int, Int) = (mockFinder.airports(plus100Times).size, plus100Times.size)

  "Topological Ordering Finder" should s"find the shortest path from DUB to SYD " +
    s"in a graph with ${graphShapeBaseLine._1} vertices and ${graphShapeBaseLine._2} edges in less than 50 ms" in {
    val initialTime = System.nanoTime()

    val graph = TopologicalOrderingFinder.buildGraph(ExampleRoutes.providedRoutes)

    val topologicalOrder = TopologicalOrderingFinder.createTopologicalOrder(graph, ExampleRoutes.providedRoutes)

    topologicalOrder.map(TopologicalOrderingFinder.findSSSP(graph, _, Airport("DUB")))

    val totalTimeMs = ((System.nanoTime() - initialTime) / 1000000).toInt

    totalTimeMs should be < 50
  }

  it should s"find the shortest path from DUB to SYD " +
    s"in a graph with ${graphShapePlus10Times._1} vertices and ${graphShapePlus10Times._2} edges in less than 200 ms" in {
    val initialTime = System.nanoTime()

    val graph = TopologicalOrderingFinder.buildGraph(plus10Times)

    val topologicalOrder = TopologicalOrderingFinder.createTopologicalOrder(graph, plus10Times)

    topologicalOrder.map(TopologicalOrderingFinder.findSSSP(graph, _, Airport("DUB")))

    val totalTimeMs = ((System.nanoTime() - initialTime) / 1000000).toInt

    totalTimeMs should be < 200
  }

  it should s"find the shortest path from DUB to SYD " +
    s"in a graph with ${graphShapePlus100Times._1} vertices and ${graphShapePlus100Times._2} edges in less than 2000 ms" in {
    val initialTime = System.nanoTime()

    val graph = TopologicalOrderingFinder.buildGraph(plus100Times)

    val topologicalOrder = TopologicalOrderingFinder.createTopologicalOrder(graph, plus100Times)

    topologicalOrder.map(TopologicalOrderingFinder.findSSSP(graph, _, Airport("DUB")))

    val totalTimeMs = ((System.nanoTime() - initialTime) / 1000000).toInt

    totalTimeMs should be < 2000
  }

  "Directed Cycle Graph Finder" should "find the shortest path from DUB to SYD " +
    s"in a graph with ${graphShapeBaseLine._1} vertices and ${graphShapeBaseLine._2} edges in less than 50 ms" in {

    val initialTime = System.nanoTime()

    DirectedCycleGraphFinder.findShortestPath(ExampleRoutes.providedRoutes, Airport("DUB"), Airport("SYD"))

    val totalTimeMs = ((System.nanoTime() - initialTime) / 1000000).toInt

    totalTimeMs should be < 1000
  }

  it should "find the shortest path from DUB to SYD " +
    s"in a graph with ${graphShapePlus10Times._1} vertices and ${graphShapePlus10Times._2} edges in less than 100 ms" in {

    val initialTime = System.nanoTime()

    DirectedCycleGraphFinder.findShortestPath(plus10Times, Airport("DUB"), Airport("SYD"))

    val totalTimeMs = ((System.nanoTime() - initialTime) / 1000000).toInt

    totalTimeMs should be < 100
  }

  it should "find the shortest path from DUB to SYD " +
    s"in a graph with ${graphShapePlus100Times._1} vertices and ${graphShapePlus100Times._2} edges in less than 1000 ms" in {

    val initialTime = System.nanoTime()

    DirectedCycleGraphFinder.findShortestPath(plus100Times, Airport("DUB"), Airport("SYD"))

    val totalTimeMs = ((System.nanoTime() - initialTime) / 1000000).toInt

    totalTimeMs should be < 1000
  }


  def plus(providedRoutes: Seq[Route], multiplier: Int): Seq[Route] = {
    val airports = mockFinder.airports(providedRoutes)

    val newInBetweenRoutes = for {
      newDeparture <- airports
      newArrival <- airports
      if newDeparture != Airport("DUB") && newArrival != Airport("SYD") &&
        newDeparture != Airport("SYD") && newArrival != Airport("DUB")
    } yield {
      (1 to multiplier).
        map(id => Route(Airport(s"${newDeparture.iataCode}$id"), Airport(s"${newArrival.iataCode}$id"), id))
    }

    val inBetweenRoutes = newInBetweenRoutes.flatMap(_.map(r => r))

    val dubReconnected = inBetweenRoutes
      .filter(newRoute => Set("ORD", "LHR", "CDG").exists(three => newRoute.departure.iataCode.startsWith(three)))
      .map(newDeparture => Route(Airport("DUB"), newDeparture.departure, util.Random.between(100, 1000)))

    val sydReconnected = newInBetweenRoutes
      .flatMap(_.map(r => r))
      .filter(newRoute => Set("LAS", "LAX", "BKK").exists(three => newRoute.departure.iataCode.startsWith(three)))
      .map(newDeparture => Route(newDeparture.departure, Airport("SYD"), util.Random.between(100, 1000)))

    (dubReconnected ++ inBetweenRoutes ++ sydReconnected).toSeq
  }
}
