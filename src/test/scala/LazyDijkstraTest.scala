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

  it should "update duration tracking to given arrival if duration at that does not exist" in {
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

  it should "update duration tracking to given arrival if duration to that is smaller its current duration" in {
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

  it should "not update duration tracking to given arrival if duration to that is bigger than its current value" in {
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

  "Lazy Dijkstra Path Finder" should "return find shortest distance all airports" in {
    val expandedRoutes = Routes.providedRoutes
    val numberOfAirports = Routes.groupAirports(expandedRoutes).size

    val graph = buildGraph(expandedRoutes)

    pending
    LazyDijkstra.dijkstra(graph, Airport("DUB"), numberOfAirports) match {
      case Success(value) => value should have size 3
      case Failure(exception) => fail(exception)
    }
  }
}
