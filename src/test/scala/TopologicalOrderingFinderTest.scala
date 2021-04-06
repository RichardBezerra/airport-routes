import com.airplanerouteschallenge.TopologicalOrderingFinder.InvalidDagCyclesFound
import com.airplanerouteschallenge._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.util.{Failure, Success}

class TopologicalOrderingFinderTest extends AnyFlatSpec with Matchers {

  "Topological Ordering Finder" should "create a topological order successfully when it receives a DAG" in {
    val graph = TopologicalOrderingFinder.buildGraph(ExampleRoutes.providedRoutes)

    val topologicalOrder = TopologicalOrderingFinder.createTopologicalOrder(graph,ExampleRoutes.providedRoutes)

    topologicalOrder.isSuccess should be(true)
  }

  it should "create a topological order with DUB as the first node and SYD as the last node" +
    " given the example routes provided" in {
    val graph = TopologicalOrderingFinder.buildGraph(ExampleRoutes.providedRoutes)

    val topologicalOrder = TopologicalOrderingFinder.createTopologicalOrder(graph,ExampleRoutes.providedRoutes)

    topologicalOrder match {
      case Success(topOrder) =>
        topOrder.head should be(Airport("DUB"))
        topOrder.last should be(Airport("SYD"))
      case Failure(_) => fail()
    }
  }

  it should "detect cycles while building topological order" in {
    val cyclicalRoutes = ExampleRoutes.providedRoutes :+ Route(Airport("LAS"), Airport("CDG"), 4)

    val graph = TopologicalOrderingFinder.buildGraph(cyclicalRoutes)

    val topOrder = TopologicalOrderingFinder.createTopologicalOrder(graph, cyclicalRoutes)

    topOrder match {
      case Failure(failure) => failure should be(InvalidDagCyclesFound)
      case Success(_) => fail()
    }
  }

  it should "find the shortest path from very beginning (DUB) to very ending (SYD)" in {
    val graph = TopologicalOrderingFinder.buildGraph(ExampleRoutes.providedRoutes)

    val topologicalOrder = TopologicalOrderingFinder.createTopologicalOrder(graph,ExampleRoutes.providedRoutes)

    val sssp = topologicalOrder.map(TopologicalOrderingFinder.findSSSP(graph, _, Airport("DUB"))).get

    sssp.last match {
      case (Airport(_), routes) =>
        routes should be(Seq(
          Route(Airport("DUB"), Airport("LHR"), 1),
          Route(Airport("LHR"), Airport("BKK"), 9),
          Route(Airport("BKK"), Airport("SYD"), 11)))
      case _ => fail()
    }
  }

  it should "find the shortest path from a source in " +
    "the middle of topological order (CDG) to an airport in the very ending (SYD)" in {
    val graph = TopologicalOrderingFinder.buildGraph(ExampleRoutes.providedRoutes)

    val topologicalOrder = TopologicalOrderingFinder.createTopologicalOrder(graph,ExampleRoutes.providedRoutes)

    val sssp = topologicalOrder.map(TopologicalOrderingFinder.findSSSP(graph, _, Airport("CDG"))).get

    sssp.last match {
      case (Airport(_), routes) =>
        routes should be(Seq(
          Route(Airport("CDG"), Airport("BKK"), 9),
          Route(Airport("BKK"), Airport("SYD"), 11)))
      case _ => fail()
    }
  }

  it should "find the shortest path from very beginning (DUB) airport " +
    "to an airport in the middle of topological ordering (LAS)" in {
    val path = TopologicalOrderingFinder.findShortestPath(ExampleRoutes.providedRoutes, Airport("DUB"), Airport("LAS"))

    path match {
      case Success(routes) =>
        routes should be(Seq(
          Route(Airport("DUB"), Airport("ORD"), 6),
          Route(Airport("ORD"), Airport("LAS"), 2)))
      case Failure(failure) => fail(failure)
    }
  }

  it should "find the shortest path from DUB to SYD" in {
    val path = TopologicalOrderingFinder.findShortestPath(ExampleRoutes.providedRoutes, Airport("DUB"), Airport("SYD"))

    path match {
      case Success(routes) =>
        routes should be(Seq(
          Route(Airport("DUB"), Airport("LHR"), 1),
          Route(Airport("LHR"), Airport("BKK"), 9),
          Route(Airport("BKK"), Airport("SYD"), 11)))
      case Failure(failure) => fail(failure)
    }
  }

  it should "find the shortest path from CDG to SYD" in {
    val path = TopologicalOrderingFinder.findShortestPath(ExampleRoutes.providedRoutes, Airport("CDG"), Airport("SYD"))

    path match {
      case Success(routes) =>
        routes should be(Seq(
          Route(Airport("CDG"), Airport("BKK"), 9),
          Route(Airport("BKK"), Airport("SYD"), 11)))
      case Failure(failure) => fail(failure)
    }
  }

  it should "return a failure when there is no path from departure to arrival" in {
    val path = TopologicalOrderingFinder.findShortestPath(ExampleRoutes.providedRoutes, Airport("LAS"), Airport("DUB"))

    path match {
      case Failure(failure) => failure should be (NoRoutesFound)
      case Success(_) => fail()
    }
  }

  it should "return a failure when arrival airport is the same as departure" in {
    val path = TopologicalOrderingFinder.findShortestPath(ExampleRoutes.providedRoutes, Airport("ORD"), Airport("ORD"))

    path match {
      case Failure(failure) => failure should be (DepartureEqualToArrival)
      case Success(_) => fail()
    }
  }

  it should "return a failure when departure airport is not in the provided list" in {
    val path = TopologicalOrderingFinder.findShortestPath(ExampleRoutes.providedRoutes, Airport("SNN"), Airport("ORD"))

    path match {
      case Failure(failure) => failure should be (InvalidAirport)
      case Success(_) => fail()
    }
  }

  it should "return a failure when arrival airport is not in the provided list" in {
    val path = TopologicalOrderingFinder.findShortestPath(ExampleRoutes.providedRoutes, Airport("DUB"), Airport("SNN"))

    path match {
      case Failure(failure) => failure should be (InvalidAirport)
      case Success(_) => fail()
    }
  }
}
