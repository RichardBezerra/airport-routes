package com.airplanerouteschallenge

import com.airplanerouteschallenge.ExampleRoutes.providedRoutes
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.util.{Failure, Success}

class DirectedCycleGraphFinderTest extends AnyFlatSpec with Matchers {

  val shortestPathFinderMock: ShortestPathFinder = (_: Seq[Route],
                                                _: Airport,
                                                _: Airport) => ???

  "DurationDistanceTrackingMap" should "be created using a list of airports" in {
    val airports = shortestPathFinderMock.airports(providedRoutes)

    val durationDistanceTrackingMap = HoursTrack(airports)

    durationDistanceTrackingMap should have size airports.size
  }

  it should "initiate departure airport duration tracking as 0" in {
    val airports = shortestPathFinderMock.airports(providedRoutes)

    val durationDistanceTrackingMap = HoursTrack(airports)

    durationDistanceTrackingMap.setDurationToZero(Airport("DUB"))

    durationDistanceTrackingMap(Airport("DUB")) should be(HoursTrackPathValue(Seq()))
  }

  it should "update duration tracking to given arrival if duration to get that does not exist" in {
    val airports = shortestPathFinderMock.airports(providedRoutes)

    val durationDistanceTrackingMap = HoursTrack(airports)

    // act
    durationDistanceTrackingMap
      .overridePathToArrivalIfRouteIsFaster(HoursTrackPathValue(), Route(Airport("DUB"), Airport("LHR"), 2))

    // assert
    durationDistanceTrackingMap(Airport("LHR")) should
      be(HoursTrackPathValue(Seq(Route(Airport("DUB"), Airport("LHR"), 2))))
  }

  it should "update duration tracking to given arrival if duration to get that is smaller then its current value" in {
    val airports = shortestPathFinderMock.airports(providedRoutes)

    val durationDistanceTrackingMap = HoursTrack(airports)

    durationDistanceTrackingMap
      .overridePathToArrivalIfRouteIsFaster(HoursTrackPathValue(),
        Route(Airport("DUB"), Airport("LHR"), 2))

    // act
    durationDistanceTrackingMap
      .overridePathToArrivalIfRouteIsFaster(HoursTrackPathValue(),
        Route(Airport("SNN"), Airport("LHR"), 1))

    // assert
    durationDistanceTrackingMap(Airport("LHR")) should
      be(HoursTrackPathValue(Seq(Route(Airport("SNN"), Airport("LHR"), 1))))
  }

  it should "update duration tracking to given arrival " +
    "if duration to get that from other path is smaller then its current value" in {
    val airports = shortestPathFinderMock.airports(providedRoutes)

    val durationDistanceTrackingMap = HoursTrack(airports)

    durationDistanceTrackingMap
      .overridePathToArrivalIfRouteIsFaster(HoursTrackPathValue(),
        Route(Airport("DUB"), Airport("SYD"), 21))

    // act
    durationDistanceTrackingMap
      .overridePathToArrivalIfRouteIsFaster(HoursTrackPathValue(),
        Route(Airport("DUB"), Airport("LHR"), 1))

    durationDistanceTrackingMap
      .overridePathToArrivalIfRouteIsFaster(durationDistanceTrackingMap(Airport("LHR")),
        Route(Airport("LHR"), Airport("SYD"), 10))

    // assert
    durationDistanceTrackingMap(Airport("SYD")) should
      be(HoursTrackPathValue(Seq(Route(Airport("DUB"), Airport("LHR"), 1),
              Route(Airport("LHR"), Airport("SYD"), 10))))
  }

  it should "not update duration tracking to given arrival if duration to get that is bigger than its current value" in {
    val airports = shortestPathFinderMock.airports(providedRoutes)

    val durationDistanceTrackingMap = HoursTrack(airports)

    durationDistanceTrackingMap.setDurationToZero(Airport("DUB"))

    durationDistanceTrackingMap
      .overridePathToArrivalIfRouteIsFaster(durationDistanceTrackingMap(Airport("DUB")),
        Route(Airport("DUB"), Airport("LHR"), 2))

    // act
    durationDistanceTrackingMap
      .overridePathToArrivalIfRouteIsFaster(durationDistanceTrackingMap(Airport("DUB")),
        Route(Airport("DUB"), Airport("LHR"), 3))

    // assert
    durationDistanceTrackingMap(Airport("LHR")) should
      be(HoursTrackPathValue(Seq(Route(Airport("DUB"),Airport("LHR"), 2))))
  }

  "Lazy Dijkstra Path Finder" should "return shortest duration to all airports" in {

    // act
    val path = DirectedCycleGraphFinder.findShortestPath(providedRoutes, Airport("DUB"), Airport("LAS"))

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
    val shortestPath = DirectedCycleGraphFinder.findShortestPath(providedRoutes, Airport("DUB"), Airport("SYD"))

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
    val expandedRoutes = addReturningRoutes(providedRoutes)

    // act
    val shortestPath = DirectedCycleGraphFinder.findShortestPath(expandedRoutes, Airport("SYD"), Airport("DUB"))

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
    val expandedRoutes = addReturningRoutes(providedRoutes)

    // act
    val shortestPath = DirectedCycleGraphFinder.findShortestPath(expandedRoutes, Airport("SYD"), Airport("DUB"))

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

    val path = DirectedCycleGraphFinder
      .findShortestPath(providedRoutes, Airport("LAS"), Airport("DUB"))

    path match {
      case Failure(failure) => failure should be (NoRoutesFound)
      case Success(_) => fail()
    }
  }

  it should "return a failure when arrival airport is the same as departure" in {

    val path = DirectedCycleGraphFinder
      .findShortestPath(providedRoutes, Airport("LAS"), Airport("LAS"))

    path match {
      case Failure(failure) => failure should be (DepartureEqualToArrival)
      case Success(_) => fail()
    }
  }

  it should "return a failure when departure airport is not in the provided list" in {

    val path = DirectedCycleGraphFinder
      .findShortestPath(providedRoutes, Airport("SNN"), Airport("LAS"))

    path match {
      case Failure(failure) => failure should be (InvalidAirport)
      case Success(_) => fail()
    }
  }
  
  it should "return a failure when arrival airport is not in the provided list" in {

    val path = DirectedCycleGraphFinder
      .findShortestPath(providedRoutes, Airport("LAS"), Airport("SNN"))

    path match {
      case Failure(failure) => failure should be (InvalidAirport)
      case Success(_) => fail()
    }
  }

  def addReturningRoutes(providedRoutes: Seq[Route]): Seq[Route] = {
    providedRoutes :++ providedRoutes
      .map(currentRoute => Route(currentRoute.arrival, currentRoute.departure, currentRoute.durationHours))
  }
}
