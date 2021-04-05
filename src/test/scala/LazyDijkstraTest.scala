import Errors.{DepartureEqualToArrival, InvalidAirport, NoRoutesFound}
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

  "DurationDistanceTrackingMap" should "be created using a list of airports" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = DurationDistanceTracking(airports)

    durationDistanceTrackingMap should have size airports.size
  }

  it should "initiate departure airport duration tracking as 0" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = DurationDistanceTracking(airports)

    durationDistanceTrackingMap.setDurationOfDepartureToZero(Airport("DUB"))

    durationDistanceTrackingMap(Airport("DUB")) should be(TrackingPath(Seq()))
  }

  it should "update duration tracking to given arrival if duration to get that does not exist" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = DurationDistanceTracking(airports)

    // act
    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(TrackingPath(),
        Routes.Route(Airport("DUB"), Airport("LHR"), 2))

    // assert
    durationDistanceTrackingMap(Airport("LHR")) should
      be(TrackingPath(Seq(Route(Airport("DUB"), Airport("LHR"), 2))))
  }

  it should "update duration tracking to given arrival if duration to get that is smaller then its current value" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = DurationDistanceTracking(airports)

    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(TrackingPath(),
        Routes.Route(Airport("DUB"), Airport("LHR"), 2))

    // act
    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(TrackingPath(),
        Routes.Route(Airport("SNN"), Airport("LHR"), 1))

    // assert
    durationDistanceTrackingMap(Airport("LHR")) should
      be(TrackingPath(Seq(Route(Airport("SNN"), Airport("LHR"), 1))))
  }

  it should "update duration tracking to given arrival " +
    "if duration to get that from other path is smaller then its current value" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = DurationDistanceTracking(airports)

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
      be(TrackingPath(Seq(Routes.Route(Airport("DUB"), Airport("LHR"), 1),
              Routes.Route(Airport("LHR"), Airport("SYD"), 10))))
  }

  it should "not update duration tracking to given arrival if duration to get that is bigger than its current value" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = DurationDistanceTracking(airports)

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
      be(TrackingPath(Seq(Route(Airport("DUB"),Airport("LHR"), 2))))
  }

  "Lazy Dijkstra Path Finder" should "return shortest duration to all airports" in {
    val expandedRoutes = Routes.providedRoutes

    val graph = buildGraph(expandedRoutes)

    // act
    LazyDijkstra.dijkstra(graph, Airport("DUB"), Airport("LAS"), Routes.groupAirports(expandedRoutes)) match {
      case Success(value) =>

        // assert
        value(Airport("LAS")) should be(TrackingPath(Seq(
                  Route(Airport("DUB"), Airport("ORD"), 6),
                  Route(Airport("ORD"), Airport("LAS"), 2)
                )))

      case Failure(exception) => fail(exception)
    }
  }

  it should "find shortest duration from a departure to an arrival" in {
    val expandedRoutes = Routes.providedRoutes

    val graph = buildGraph(expandedRoutes)

    val allAirports = Routes.groupAirports(expandedRoutes)

    val dijkstra = LazyDijkstra.dijkstra(graph, Airport("DUB"), Airport("SYD"), allAirports).get

    // act
    val shortestPath = LazyDijkstra.findShortestPath(graph, Airport("DUB"), Airport("SYD"), allAirports, dijkstra)

    // assert
    shortestPath match {
      case Success((shortestPath, duration)) =>
        shortestPath should be(
          Seq(Route(Airport("DUB"), Airport("LHR"), 1),
            Route(Airport("LHR"), Airport("BKK"), 9),
            Route(Airport("BKK"), Airport("SYD"), 11)))

        duration should be(21)

      case Failure(failure) => fail(failure)
    }
  }

  it should "find shortest duration from a departure to an arrival when routes are expanded " +
    "to include returning routes" in {
    val expandedRoutes = addReturningRoutes(Routes.providedRoutes)

    val allAirports = Routes.groupAirports(expandedRoutes)

    val directedCyclicalGraph = buildGraph(expandedRoutes)

    val dijkstra = LazyDijkstra.dijkstra(directedCyclicalGraph, Airport("SYD"), Airport("DUB"), allAirports).get

    // act
    val shortestPath = LazyDijkstra.findShortestPath(directedCyclicalGraph, Airport("SYD"), Airport("DUB"), allAirports, dijkstra)

    // assert
    shortestPath match {
      case Success((shortestPath, duration)) =>
        shortestPath should be(
          Seq(Route(Airport("SYD"), Airport("BKK"), 11),
            Route(Airport("BKK"), Airport("LHR"), 9),
            Route(Airport("LHR"), Airport("DUB"), 1)))

        duration should be(21)

      case Failure(failure) => fail(failure)
    }
  }

  it should "return a failure when arrival airport is unreachable from the departure" in {
    val allAirports = Routes.groupAirports(Routes.providedRoutes)

    val graph = buildGraph(Routes.providedRoutes)

    val dijkstra = LazyDijkstra.dijkstra(graph, Airport("LAS"), Airport("DUB"), allAirports).get

    val path = LazyDijkstra.findShortestPath(graph, Airport("LAS"), Airport("DUB"), allAirports, dijkstra)

    path match {
      case Failure(failure) => failure should be (NoRoutesFound)
      case Success(_) => fail()
    }
  }

  it should "return a failure when arrival airport is the same as departure" in {
    val path = LazyDijkstra.findShortestPath(null, Airport("LAS"), Airport("LAS"), Set(), null)

    path match {
      case Failure(failure) => failure should be (DepartureEqualToArrival)
      case Success(_) => fail()
    }
  }

  it should "return a failure when departure airport is not in the provided list" in {
    pending

    val path = LazyDijkstra.findShortestPath(null, Airport("SNN"), Airport("LAS"), Set(), null)

    path match {
      case Failure(failure) => failure should be (InvalidAirport)
      case Success(_) => fail()
    }
  }
  
  it should "return a failure when arrival airport is not in the provided list" in {
    pending

    val path = LazyDijkstra.findShortestPath(null, Airport("LAS"), Airport("SNN"), Set(), null)

    path match {
      case Failure(failure) => failure should be (InvalidAirport)
      case Success(_) => fail()
    }
  }

  def addReturningRoutes(providedRoutes: Seq[Routes.Route]): Seq[Routes.Route] = {
    providedRoutes :++ providedRoutes
      .map(currentRoute => Routes.Route(currentRoute.arrival, currentRoute.departure, currentRoute.durationHours))
  }
}
