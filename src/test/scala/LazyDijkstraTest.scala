import Routes.{Airport, Route, buildGraph, providedRoutes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable
import scala.util.{Failure, Success}

class LazyDijkstraTest extends AnyFlatSpec with Matchers {
  "RouteDurationReverseOrdering" should "enqueue items following priority" in {
    val routesPQ = mutable.PriorityQueue()(RouteDurationReverseOrdering)

    providedRoutes.foreach(r => routesPQ.enqueue((r.arrival, TrackingPath(Seq(r)))))

    routesPQ.dequeue()._2.totalDuration should be(1)

    routesPQ.dequeue()._2.totalDuration should be(2)

    routesPQ.enqueue((Airport("A"), TrackingPath(Seq(Routes.Route(Airport("A"), Airport("B"), 1)))))

    routesPQ.dequeue()._2.totalDuration should be(1)
  }

  "TrackingPath" should "add route when is not initiated" in {

  }

  it should "add route when is final duration is smaller than its current one" in {

  }

  "DurationDistanceTrackingMap" should "be created using a list of airports" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = DurationDistanceTrackingMap(airports)

    durationDistanceTrackingMap should have size airports.size
  }

  it should "initiate departure airport duration tracking as 0" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = DurationDistanceTrackingMap(airports)

    durationDistanceTrackingMap.setDurationOfDepartureToZero(Airport("DUB"))

    durationDistanceTrackingMap(Airport("DUB")) should be(TrackingPath(isInitiated = false, Seq()))
  }

  it should "update duration tracking to given arrival if duration to get that does not exist" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = DurationDistanceTrackingMap(airports)

    // act
    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(TrackingPath(),
        Routes.Route(Airport("DUB"), Airport("LHR"), 2))

    // assert
    durationDistanceTrackingMap(Airport("LHR")) should
      be(TrackingPath(isInitiated = true, Seq(Route(Airport("DUB"), Airport("LHR"), 2))))
  }

  it should "update duration tracking to given arrival if duration to get that is smaller then its current value" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = DurationDistanceTrackingMap(airports)

    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(TrackingPath(),
        Routes.Route(Airport("DUB"), Airport("LHR"), 2))

    // act
    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(TrackingPath(),
        Routes.Route(Airport("SNN"), Airport("LHR"), 1))

    // assert
    durationDistanceTrackingMap(Airport("LHR")) should
      be(TrackingPath(isInitiated = true, Seq(Route(Airport("SNN"), Airport("LHR"), 1))))
  }

  it should "update duration tracking to given arrival " +
    "if duration to get that from other path is smaller then its current value" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = DurationDistanceTrackingMap(airports)

    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(TrackingPath(),
        Routes.Route(Airport("DUB"), Airport("SYD"), 21))

    // act
    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(TrackingPath(),
        Routes.Route(Airport("DUB"), Airport("LHR"), 1))

    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(durationDistanceTrackingMap(Airport("LHR")),
        Routes.Route(Airport("LHR"), Airport("SYD"), 10))

    // assert
    durationDistanceTrackingMap(Airport("SYD")) should
      be(TrackingPath(isInitiated = true, Seq(Routes.Route(Airport("DUB"), Airport("LHR"), 1),
        Routes.Route(Airport("LHR"), Airport("SYD"), 10))))
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
    durationDistanceTrackingMap(Airport("LHR")) should
      be(TrackingPath(isInitiated = true, Seq(Route(Airport("DUB"),Airport("LHR"), 2))))
  }

  "Lazy Dijkstra Path Finder" should "return shortest distance to all airports" in {
    val expandedRoutes = Routes.providedRoutes
    val numberOfAirports = Routes.groupAirports(expandedRoutes).size

    val graph = buildGraph(expandedRoutes)

    LazyDijkstra.dijkstra(graph, Airport("DUB"), numberOfAirports) match {
      case Success(value) =>
        value(Airport("SYD")) should be(TrackingPath(isInitiated = true, Seq(
          Route(Airport("DUB"), Airport("LHR"), 1),
          Route(Airport("LHR"), Airport("BKK"), 9),
          Route(Airport("BKK"), Airport("SYD"), 11)
        )))
      case Failure(exception) => fail(exception)
    }
  }
}
