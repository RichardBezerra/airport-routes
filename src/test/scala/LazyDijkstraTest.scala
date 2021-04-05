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

    providedRoutes.foreach(r => routesPQ.enqueue((r.arrival, HoursTrackPathValue(Seq(r)))))

    routesPQ.dequeue()._2.totalDuration should be(1)

    routesPQ.dequeue()._2.totalDuration should be(2)

    routesPQ.enqueue((Airport("A"), HoursTrackPathValue(Seq(Routes.Route(Airport("A"), Airport("B"), 1)))))

    routesPQ.dequeue()._2.totalDuration should be(1)
  }

  "DurationDistanceTrackingMap" should "be created using a list of airports" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = HoursTrack(airports)

    durationDistanceTrackingMap should have size airports.size
  }

  it should "initiate departure airport duration tracking as 0" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = HoursTrack(airports)

    durationDistanceTrackingMap.setDurationOfDepartureToZero(Airport("DUB"))

    durationDistanceTrackingMap(Airport("DUB")) should be(HoursTrackPathValue(Seq()))
  }

  it should "update duration tracking to given arrival if duration to get that does not exist" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = HoursTrack(airports)

    // act
    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(HoursTrackPathValue(),
        Routes.Route(Airport("DUB"), Airport("LHR"), 2))

    // assert
    durationDistanceTrackingMap(Airport("LHR")) should
      be(HoursTrackPathValue(Seq(Route(Airport("DUB"), Airport("LHR"), 2))))
  }

  it should "update duration tracking to given arrival if duration to get that is smaller then its current value" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = HoursTrack(airports)

    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(HoursTrackPathValue(),
        Routes.Route(Airport("DUB"), Airport("LHR"), 2))

    // act
    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(HoursTrackPathValue(),
        Routes.Route(Airport("SNN"), Airport("LHR"), 1))

    // assert
    durationDistanceTrackingMap(Airport("LHR")) should
      be(HoursTrackPathValue(Seq(Route(Airport("SNN"), Airport("LHR"), 1))))
  }

  it should "update duration tracking to given arrival " +
    "if duration to get that from other path is smaller then its current value" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = HoursTrack(airports)

    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(HoursTrackPathValue(),
        Routes.Route(Airport("DUB"), Airport("SYD"), 21))

    // act
    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(HoursTrackPathValue(),
        Routes.Route(Airport("DUB"), Airport("LHR"), 1))

    durationDistanceTrackingMap
      .reduceDurationToArrivalIfRouteIsFaster(durationDistanceTrackingMap(Airport("LHR")),
        Routes.Route(Airport("LHR"), Airport("SYD"), 10))

    // assert
    durationDistanceTrackingMap(Airport("SYD")) should
      be(HoursTrackPathValue(Seq(Routes.Route(Airport("DUB"), Airport("LHR"), 1),
              Routes.Route(Airport("LHR"), Airport("SYD"), 10))))
  }

  it should "not update duration tracking to given arrival if duration to get that is bigger than its current value" in {
    val airports = Routes.groupAirports(providedRoutes)

    val durationDistanceTrackingMap = HoursTrack(airports)

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
      be(HoursTrackPathValue(Seq(Route(Airport("DUB"),Airport("LHR"), 2))))
  }

  "Lazy Dijkstra Path Finder" should "return shortest duration to all airports" in {

    // act
    val path = LazyDijkstra
      .findShortestPath(Routes.providedRoutes, Airport("DUB"), Airport("LAS"))(LazyDijkstra)

    // assert
    path match {
      case Success(routes) =>
        routes should be(Seq(
          Route(Airport("DUB"), Airport("ORD"), 6),
          Route(Airport("ORD"), Airport("LAS"), 2)
        ))

      case Failure(exception) => fail(exception)
    }
  }

  it should "find shortest duration from a departure to an arrival" in {

    // act
    val shortestPath = LazyDijkstra
      .findShortestPath(Routes.providedRoutes, Airport("DUB"), Airport("SYD"))(LazyDijkstra)

    // assert
    shortestPath match {
      case Success(routes) =>
        routes should be(
          Seq(Route(Airport("DUB"), Airport("LHR"), 1),
            Route(Airport("LHR"), Airport("BKK"), 9),
            Route(Airport("BKK"), Airport("SYD"), 11)))

      case Failure(failure) => fail(failure)
    }
  }

  it should "find shortest duration from a departure to an arrival when routes are expanded " +
    "to include returning routes" in {
    val expandedRoutes = addReturningRoutes(Routes.providedRoutes)

    // act
    val shortestPath = LazyDijkstra
      .findShortestPath(expandedRoutes, Airport("SYD"), Airport("DUB"))(LazyDijkstra)

    // assert
    shortestPath match {
      case Success(routes) =>
        routes should be(
          Seq(Route(Airport("SYD"), Airport("BKK"), 11),
            Route(Airport("BKK"), Airport("LHR"), 9),
            Route(Airport("LHR"), Airport("DUB"), 1)))

      case Failure(failure) => fail(failure)
    }
  }

  "ShortestPathFinder" should "find shortest duration from a departure to an arrival when routes are expanded " +
    "to include returning routes" in {
    val expandedRoutes = addReturningRoutes(Routes.providedRoutes)

    // act
    val shortestPath = LazyDijkstra.findShortestPath(expandedRoutes, Airport("SYD"), Airport("DUB"))(LazyDijkstra)

    // assert
    shortestPath match {
      case Success(shortestPath) =>
        shortestPath should be(
          Seq(Route(Airport("SYD"), Airport("BKK"), 11),
            Route(Airport("BKK"), Airport("LHR"), 9),
            Route(Airport("LHR"), Airport("DUB"), 1)))

      case Failure(failure) => fail(failure)
    }
  }

  it should "return a failure when arrival airport is unreachable from the departure" in {
    val finder = new ShortestPathFinder { }

    val path = finder.findShortestPath(Routes.providedRoutes,
      Airport("DUB"),
      Airport("LAS"))((_: Map[Airport, Seq[Route]],
                       _: Set[Airport],
                       _: Airport,
                       _: Airport,
                       _: mutable.PriorityQueue[(Airport, HoursTrackPathValue)],
                       _: HoursTrack) => { })

    path match {
      case Failure(failure) => failure should be (NoRoutesFound)
      case Success(_) => fail()
    }
  }

  it should "return a failure when arrival airport is the same as departure" in {
    val finder = new ShortestPathFinder { }

    val path = finder.findShortestPath(Routes.providedRoutes,
      Airport("LAS"),
      Airport("LAS"))((_: Map[Airport, Seq[Route]],
                       _: Set[Airport],
                       _: Airport,
                       _: Airport,
                       _: mutable.PriorityQueue[(Airport, HoursTrackPathValue)],
                       _: HoursTrack) => ???)

    path match {
      case Failure(failure) => failure should be (DepartureEqualToArrival)
      case Success(_) => fail()
    }
  }

  it should "return a failure when departure airport is not in the provided list" in {
    val finder = new ShortestPathFinder { }

    val path = finder.findShortestPath(Routes.providedRoutes,
      Airport("SNN"),
      Airport("LAS"))((_: Map[Airport, Seq[Route]],
                       _: Set[Airport],
                       _: Airport,
                       _: Airport,
                       _: mutable.PriorityQueue[(Airport, HoursTrackPathValue)],
                       _: HoursTrack) => ???)

    path match {
      case Failure(failure) => failure should be (InvalidAirport)
      case Success(_) => fail()
    }
  }
  
  it should "return a failure when arrival airport is not in the provided list" in {
    val finder = new ShortestPathFinder { }

    val path = finder.findShortestPath(Routes.providedRoutes,
      Airport("LAS"),
      Airport("SNN"))((_: Map[Airport, Seq[Route]],
                       _: Set[Airport],
                       _: Airport,
                       _: Airport,
                       _: mutable.PriorityQueue[(Airport, HoursTrackPathValue)],
                       _: HoursTrack) => ???)

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
