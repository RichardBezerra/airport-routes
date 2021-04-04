import Routes.{Airport, buildGraph, providedRoutes}
import org.scalatest.Entry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import java.util.Map.entry
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

  "DurationDistanceTrackingMap" should "be created using a list of airports" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = DurationDistanceTrackingMap(airports)

    durationDistanceTrackingMap should have size(airports.size)
  }

  it should "initiate departure airport duration tracking as 0" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = DurationDistanceTrackingMap(airports)

    durationDistanceTrackingMap.setDurationOfDepartureToZero(Airport("DUB"))

    durationDistanceTrackingMap(Airport("DUB")) should be(Some(0))
  }

  it should "update duration tracking to given arrival if duration at get that does not exist" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = DurationDistanceTrackingMap(airports)

    durationDistanceTrackingMap.setDurationOfDepartureToZero(Airport("DUB"))

    // act
    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(durationDistanceTrackingMap(Airport("DUB")),
        Routes.Route(Airport("DUB"), Airport("LHR"), 2))

    // assert
    durationDistanceTrackingMap(Airport("LHR")) should be(Some(2))
  }

  it should "update duration tracking to given arrival if duration to get that is smaller then its current value" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = DurationDistanceTrackingMap(airports)

    durationDistanceTrackingMap.setDurationOfDepartureToZero(Airport("DUB"))

    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(durationDistanceTrackingMap(Airport("DUB")),
        Routes.Route(Airport("DUB"), Airport("LHR"), 2))

    // act
    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(durationDistanceTrackingMap(Airport("DUB")),
        Routes.Route(Airport("DUB"), Airport("LHR"), 1))

    // assert
    durationDistanceTrackingMap(Airport("LHR")) should be(Some(1))
  }

  it should "not update duration tracking to given arrival if duration to get that is bigger than its current value" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = DurationDistanceTrackingMap(airports)

    durationDistanceTrackingMap.setDurationOfDepartureToZero(Airport("DUB"))

    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(durationDistanceTrackingMap(Airport("DUB")),
        Routes.Route(Airport("DUB"), Airport("LHR"), 2))

    // act
    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(durationDistanceTrackingMap(Airport("DUB")),
        Routes.Route(Airport("DUB"), Airport("LHR"), 3))

    // assert
    durationDistanceTrackingMap(Airport("LHR")) should be(Some(2))
  }

  it should "generate expected map based on control routes" in {
    val subsetRoutes = Seq(
      Routes.Route(Airport("DUB"), Airport("LHR"), 4),
      Routes.Route(Airport("DUB"), Airport("CDG"), 1),
      Routes.Route(Airport("CDG"), Airport("LHR"), 2),
      Routes.Route(Airport("CDG"), Airport("BKK"), 5),
      Routes.Route(Airport("LHR"), Airport("BKK"), 1),
      Routes.Route(Airport("BKK"), Airport("LAX"), 3)
    )

    val airports = Routes.groupAirports(subsetRoutes)

    val durationDistanceTrackingMap = DurationDistanceTrackingMap(airports)

    durationDistanceTrackingMap.setDurationOfDepartureToZero(Airport("DUB"))

    durationDistanceTrackingMap.reduceDurationToArrivalIfRouteIsFaster(Some(0), Routes.Route(Airport("DUB"), Airport("LHR"), 4))
    durationDistanceTrackingMap.reduceDurationToArrivalIfRouteIsFaster(Some(0), Routes.Route(Airport("DUB"), Airport("CDG"), 1))
    durationDistanceTrackingMap.reduceDurationToArrivalIfRouteIsFaster(Some(1), Routes.Route(Airport("CDG"), Airport("LHR"), 2))
    durationDistanceTrackingMap.reduceDurationToArrivalIfRouteIsFaster(Some(1), Routes.Route(Airport("CDG"), Airport("BKK"), 5))
    durationDistanceTrackingMap.reduceDurationToArrivalIfRouteIsFaster(Some(3), Routes.Route(Airport("LHR"), Airport("BKK"), 1))
    durationDistanceTrackingMap.reduceDurationToArrivalIfRouteIsFaster(Some(4), Routes.Route(Airport("BKK"), Airport("LAX"), 3))

    durationDistanceTrackingMap(Airport("DUB")) should be(Some(0))
    durationDistanceTrackingMap(Airport("LHR")) should be(Some(3))
    durationDistanceTrackingMap(Airport("CDG")) should be(Some(1))
    durationDistanceTrackingMap(Airport("BKK")) should be(Some(4))
    durationDistanceTrackingMap(Airport("LAX")) should be(Some(7))
  }

  "Lazy Dijkstra Path Finder" should "return shortest distance to all airports" in {
    val expandedRoutes = Routes.providedRoutes
    val numberOfAirports = Routes.groupAirports(expandedRoutes).size

    val graph = buildGraph(expandedRoutes)

    LazyDijkstra.dijkstra(graph, Airport("DUB"), numberOfAirports) match {
      case Success(value) => value(Airport("SYD")) should be(Some(21))
      case Failure(exception) => fail(exception)
    }
  }
}
