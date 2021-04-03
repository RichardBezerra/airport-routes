import Routes.{Airport, buildGraph}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.util.{Failure, Success}

class SingleSourceShortestPathTest extends AnyFlatSpec with Matchers {

  "Single Source Shortest Path" should "create the topological order successfully when it receives a DAG" in {
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

    val topologicalOrder = SingleSourceShortestPath.createTopologicalOrder(cyclicalRoutes)

    topologicalOrder.isFailure should be(true)
  }

  it should "return routes when arrival airport is reachable" in {
    pending
  }

  it should "not return routes when arrival airport is unreachable" in {
    pending
  }

  it should "not return routes when arrival airport is the same as departure" in {
    pending
  }
}
