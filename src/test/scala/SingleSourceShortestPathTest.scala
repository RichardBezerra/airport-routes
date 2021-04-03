import Routes.Airport
import SingleSourceShortestPath.{DepartureEqualToArrival, InvalidDagCyclesFound, NoRoutesFound}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.util.{Failure, Success}

class SingleSourceShortestPathTest extends AnyFlatSpec with Matchers {

  "Single Source Shortest Path" should "create a topological order successfully when it receives a DAG" in {
    val topologicalOrder = SingleSourceShortestPath.createTopologicalOrder(Routes.providedRoutes)

    topologicalOrder.isSuccess should be(true)
  }

  it should "create a topological order with DUB as the first and SYD as the last given the provided routes" in {
    val topologicalOrder = SingleSourceShortestPath.createTopologicalOrder(Routes.providedRoutes)

    topologicalOrder match {
      case Success(topOrder) =>
        topOrder.head should be(Airport("DUB"))
        topOrder.last should be(Airport("SYD"))
      case Failure(_) => fail()
    }
  }

  it should "detect cycles while building topological order" in {
    val cyclicalRoutes = Routes.providedRoutes :+ Routes.Route(Airport("LAS"), Airport("CDG"), 4)

    val topOrder = SingleSourceShortestPath.createTopologicalOrder(cyclicalRoutes)

    topOrder match {
      case Failure(failure) => failure should be(InvalidDagCyclesFound)
      case Success(_) => fail()
    }
  }

  it should "create the single shortest path from DUB to SYD" in {
    val graph = Routes.buildGraph(Routes.providedRoutes)

    val topologicalOrder = SingleSourceShortestPath.createTopologicalOrder(Routes.providedRoutes)

    val sssp = topologicalOrder.map(SingleSourceShortestPath.createSSSP(graph, _, Airport("DUB"))).get

    sssp.last match {
      case (Airport(airport), hours) =>
        airport should be("SYD")
        hours should be(Some(21))
      case _ => fail()
    }
  }

  it should "create the single shortest path from CDG to SYD" in {
    val graph = Routes.buildGraph(Routes.providedRoutes)

    val topologicalOrder = SingleSourceShortestPath.createTopologicalOrder(Routes.providedRoutes)

    val sssp = topologicalOrder.map(SingleSourceShortestPath.createSSSP(graph, _, Airport("CDG"))).get

    sssp.last match {
      case (Airport(airport), hours) =>
        airport should be("SYD")
        hours should be(Some(20))
      case _ => fail()
    }
  }

  it should "find the shortest path from DUB to LAS" in {
    val path = SingleSourceShortestPath.findShortestPath(Airport("DUB"), Airport("LAS"), Routes.providedRoutes).get

    print(path)

    path.last match {
      case (Airport(airport), hours) =>
        airport should be("LAS")
        hours should be(Some(8))
      case _ => fail()
    }
  }

  it should "find the shortest path from DUB to SYD" in {
    val path = SingleSourceShortestPath.findShortestPath(Airport("DUB"), Airport("SYD"), Routes.providedRoutes).get

    path.last match {
      case (Airport(airport), hours) =>
        airport should be("SYD")
        hours should be(Some(21))
      case _ => fail()
    }
  }

  it should "find the shortest path from CDG to SYD" in {
    val path = SingleSourceShortestPath.findShortestPath(Airport("CDG"), Airport("SYD"), Routes.providedRoutes).get

    path.last match {
      case (Airport(airport), hours) =>
        airport should be("SYD")
        hours should be(Some(20))
      case _ => fail()
    }
  }

  it should "not return routes when arrival airport is unreachable" in {
    val path = SingleSourceShortestPath.findShortestPath(Airport("LAS"), Airport("DUB"), Routes.providedRoutes)

    path match {
      case Failure(failure) => failure should be (NoRoutesFound)
      case Success(_) => fail()
    }
  }

  it should "not return routes when arrival airport is the same as departure" in {
    val path = SingleSourceShortestPath.findShortestPath(Airport("ORD"), Airport("ORD"), Routes.providedRoutes)

    path match {
      case Failure(failure) => failure should be (DepartureEqualToArrival)
      case Success(_) => fail()
    }
  }
}
