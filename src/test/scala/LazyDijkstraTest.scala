import Routes.{Airport, buildGraph, providedRoutes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable
import scala.util.{Failure, Success}

class LazyDijkstraTest extends AnyFlatSpec with Matchers {
  "RouteDurationReverseOrdering" should "enqueue items following priority" in {
    val routesPQ = mutable.PriorityQueue()(RouteDurationReverseOrdering)

    providedRoutes.foreach(routesPQ.enqueue(_))

    routesPQ.dequeue().durationHours should be(1)

    routesPQ.dequeue().durationHours should be(2)

    routesPQ.enqueue(Routes.Route(Airport("A"), Airport("B"), 1))

    routesPQ.dequeue().durationHours should be(1)
  }

  "Lazy Dijkstra Path Finder" should "return find shortest distance all airports" in {
    val expandedRoutes = Routes.providedRoutes
    val numberOfAirports = Routes.groupAirports(expandedRoutes).size

    val graph = buildGraph(expandedRoutes)

    LazyDijkstra.dijkstra(graph, Airport("DUB"), numberOfAirports) match {
      case Success(value) => value should have size 3
      case Failure(exception) => fail(exception)
    }
  }
}
