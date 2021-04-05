import Errors.{DepartureEqualToArrival, InvalidAirport, NoRoutesFound}
import Routes.{Airport, Route, buildGraph}
import SingleSourceShortestPath.InvalidDagCyclesFound
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.util.{Failure, Success}

class SingleSourceShortestPathTest extends AnyFlatSpec with Matchers {

  "Single Source Shortest Path" should "create a topological order successfully when it receives a DAG" in {
    val graph = buildGraph(Routes.providedRoutes)

    val topologicalOrder = SingleSourceShortestPath.createTopologicalOrder(graph, Routes.providedRoutes)

    topologicalOrder.isSuccess should be(true)
  }

  it should "create a topological order with DUB as the first and SYD as the last given the provided routes" in {
    val graph = buildGraph(Routes.providedRoutes)

    val topologicalOrder = SingleSourceShortestPath.createTopologicalOrder(graph, Routes.providedRoutes)

    topologicalOrder match {
      case Success(topOrder) =>
        topOrder.head should be(Airport("DUB"))
        topOrder.last should be(Airport("SYD"))
      case Failure(_) => fail()
    }
  }

  it should "detect cycles while building topological order" in {
    val cyclicalRoutes = Routes.providedRoutes :+ Routes.Route(Airport("LAS"), Airport("CDG"), 4)

    val graph = buildGraph(cyclicalRoutes)

    val topOrder = SingleSourceShortestPath.createTopologicalOrder(graph, cyclicalRoutes)

    topOrder match {
      case Failure(failure) => failure should be(InvalidDagCyclesFound)
      case Success(_) => fail()
    }
  }

  it should "create the single shortest path from DUB to SYD" in {
    val graph = Routes.buildGraph(Routes.providedRoutes)

    val topologicalOrder = SingleSourceShortestPath.createTopologicalOrder(graph, Routes.providedRoutes)

    val sssp = topologicalOrder.map(SingleSourceShortestPath.createSSSP(graph, _, Airport("DUB"))).get

    sssp.last match {
      case (Airport(_), routes) =>
        routes should be(Seq(
          Route(Airport("DUB"), Airport("LHR"), 1),
          Route(Airport("LHR"), Airport("BKK"), 9),
          Route(Airport("BKK"), Airport("SYD"), 11)))
      case _ => fail()
    }
  }

  it should "create the single shortest path from CDG to SYD" in {
    val graph = Routes.buildGraph(Routes.providedRoutes)

    val topologicalOrder = SingleSourceShortestPath.createTopologicalOrder(graph, Routes.providedRoutes)

    val sssp = topologicalOrder.map(SingleSourceShortestPath.createSSSP(graph, _, Airport("CDG"))).get

    sssp.last match {
      case (Airport(_), routes) =>
        routes should be(Seq(
          Route(Airport("CDG"), Airport("BKK"), 9),
          Route(Airport("BKK"), Airport("SYD"), 11)))
      case _ => fail()
    }
  }

  it should "find the shortest path from DUB to LAS" in {
    val path = SingleSourceShortestPath.findShortestPath(Airport("DUB"), Airport("LAS"), Routes.providedRoutes)

    path match {
      case Success(routes) =>
        routes should be(Seq(
          Route(Airport("DUB"), Airport("ORD"), 6),
          Route(Airport("ORD"), Airport("LAS"), 2)))
      case Failure(failure) => fail(failure)
    }
  }

  it should "find the shortest path from DUB to SYD" in {
    val path = SingleSourceShortestPath.findShortestPath(Airport("DUB"), Airport("SYD"), Routes.providedRoutes)

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
    val path = SingleSourceShortestPath.findShortestPath(Airport("CDG"), Airport("SYD"), Routes.providedRoutes)

    path match {
      case Success(routes) =>
        routes should be(Seq(
          Route(Airport("CDG"), Airport("BKK"), 9),
          Route(Airport("BKK"), Airport("SYD"), 11)))
      case Failure(failure) => fail(failure)
    }
  }

  it should "return a failure when arrival airport is unreachable from the departure" in {
    val path = SingleSourceShortestPath.findShortestPath(Airport("LAS"), Airport("DUB"), Routes.providedRoutes)

    path match {
      case Failure(failure) => failure should be (NoRoutesFound)
      case Success(_) => fail()
    }
  }

  it should "return a failure when arrival airport is the same as departure" in {
    val path = SingleSourceShortestPath.findShortestPath(Airport("ORD"), Airport("ORD"), Routes.providedRoutes)

    path match {
      case Failure(failure) => failure should be (DepartureEqualToArrival)
      case Success(_) => fail()
    }
  }

  it should "return a failure when departure airport is not in the provided list" in {
    val path = SingleSourceShortestPath.findShortestPath(Airport("SNN"), Airport("ORD"), Routes.providedRoutes)

    path match {
      case Failure(failure) => failure should be (InvalidAirport)
      case Success(_) => fail()
    }
  }

  it should "return a failure when arrival airport is not in the provided list" in {
    val path = SingleSourceShortestPath.findShortestPath(Airport("DUB"), Airport("SNN"), Routes.providedRoutes)

    path match {
      case Failure(failure) => failure should be (InvalidAirport)
      case Success(_) => fail()
    }
  }
}
