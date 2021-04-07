package com.airplanerouteschallenge

import com.airplanerouteschallenge.ExampleRoutes.{extendedRoutes, providedRoutes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.util.{Failure, Success}

class DirectedCycleGraphFinderTest extends AnyFlatSpec with Matchers {

  val shortestPathFinderMock: ShortestPathFinder = (_: Seq[Route],
                                                _: Airport,
                                                _: Airport) => ???

  "Directed Cycle Graph Finder" should "find shortest duration from a departure to an arrival" in {

    // act
    val shortestPath = DirectedCycleGraphFinder.findShortestPath(providedRoutes, Airport("DUB"), Airport("SYD"))

    // assert
    shortestPath match {
      case Success(shortestPath) =>
        shortestPath should be(
          Seq(Route(Airport("DUB"), Airport("LHR"), 1),
            Route(Airport("LHR"), Airport("BKK"), 9),
            Route(Airport("BKK"), Airport("SYD"), 11)))

      case Failure(failure) => fail(failure)
    }
  }

  it should "find shortest duration from a departure to an arrival " +
    "when routes are extended to include returning routes" in {

    // act
    val shortestPath = DirectedCycleGraphFinder.findShortestPath(extendedRoutes, Airport("SYD"), Airport("DUB"))

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

  it should "return a failure when there is not route from departure to arrival" in {

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
      case Failure(failure) => failure shouldBe a [DepartureEqualToArrival]
      case Success(_) => fail()
    }
  }

  it should "return a failure when departure airport is not in the provided list" in {

    val path = DirectedCycleGraphFinder
      .findShortestPath(providedRoutes, Airport("SNN"), Airport("LAS"))

    path match {
      case Failure(failure) => failure shouldBe a [InvalidAirport]
      case Success(_) => fail()
    }
  }

  it should "return a failure when arrival airport is not in the provided list" in {

    val path = DirectedCycleGraphFinder
      .findShortestPath(providedRoutes, Airport("LAS"), Airport("SNN"))

    path match {
      case Failure(failure) => failure shouldBe a [InvalidAirport]
      case Success(_) => fail()
    }
  }

  it should "provide a Dijkstra implementation that fills shortest duration for some airports " +
    "until it gets to arrival" in {
    val graph = shortestPathFinderMock.buildGraph(providedRoutes)

    val allAirports = shortestPathFinderMock.airports(providedRoutes)

    val hoursDistanceTracking = HoursTrack(allAirports)

    hoursDistanceTracking.setDurationToZero(Airport("DUB"))

    // act
    LazyDijkstra.fillHoursTrack(graph, allAirports, Airport("DUB"), Airport("LAS"), hoursDistanceTracking)

    // assert
    hoursDistanceTracking(Airport("LAS")).routes should have size 2
    hoursDistanceTracking(Airport("LAX")).routes should have size 0
  }

  "Hours Track" should "be created using a list of airports" in {
    val airports = shortestPathFinderMock.airports(providedRoutes)

    val hoursTrack = HoursTrack(airports)

    hoursTrack should have size airports.size
  }

  it should "initiate departure airport duration tracking as 0" in {
    val airports = shortestPathFinderMock.airports(providedRoutes)

    val hoursTrack = HoursTrack(airports)

    hoursTrack.setDurationToZero(Airport("DUB"))

    hoursTrack(Airport("DUB")) should be(HoursTrackPathValue(Seq()))
  }

  it should "update duration tracking to given arrival if duration to get that does not exist" in {
    val airports = shortestPathFinderMock.airports(providedRoutes)

    val hoursTrack = HoursTrack(airports)

    // act
    hoursTrack
      .overridePathToArrivalIfRouteIsFaster(HoursTrackPathValue(), Route(Airport("DUB"), Airport("LHR"), 2))

    // assert
    hoursTrack(Airport("LHR")) should
      be(HoursTrackPathValue(Seq(Route(Airport("DUB"), Airport("LHR"), 2))))
  }

  it should "update duration tracking to given arrival if duration to get that is smaller then its current value" in {
    val airports = shortestPathFinderMock.airports(providedRoutes)

    val hoursTrack = HoursTrack(airports)

    hoursTrack
      .overridePathToArrivalIfRouteIsFaster(HoursTrackPathValue(),
        Route(Airport("DUB"), Airport("LHR"), 2))

    // act
    hoursTrack
      .overridePathToArrivalIfRouteIsFaster(HoursTrackPathValue(),
        Route(Airport("SNN"), Airport("LHR"), 1))

    // assert
    hoursTrack(Airport("LHR")) should
      be(HoursTrackPathValue(Seq(Route(Airport("SNN"), Airport("LHR"), 1))))
  }

  it should "update duration tracking to given arrival " +
    "if duration to get that from other path is smaller then its current value" in {
    val airports = shortestPathFinderMock.airports(providedRoutes)

    val hoursTrack = HoursTrack(airports)

    hoursTrack
      .overridePathToArrivalIfRouteIsFaster(HoursTrackPathValue(),
        Route(Airport("DUB"), Airport("SYD"), 21))

    // act
    hoursTrack
      .overridePathToArrivalIfRouteIsFaster(HoursTrackPathValue(),
        Route(Airport("DUB"), Airport("LHR"), 1))

    hoursTrack
      .overridePathToArrivalIfRouteIsFaster(hoursTrack(Airport("LHR")),
        Route(Airport("LHR"), Airport("SYD"), 10))

    // assert
    hoursTrack(Airport("SYD")) should
      be(HoursTrackPathValue(Seq(Route(Airport("DUB"), Airport("LHR"), 1),
        Route(Airport("LHR"), Airport("SYD"), 10))))
  }

  it should "not update duration tracking to given arrival if duration to get that is bigger than its current value" in {
    val airports = shortestPathFinderMock.airports(providedRoutes)

    val hoursTrack = HoursTrack(airports)

    hoursTrack.setDurationToZero(Airport("DUB"))

    hoursTrack
      .overridePathToArrivalIfRouteIsFaster(hoursTrack(Airport("DUB")),
        Route(Airport("DUB"), Airport("LHR"), 2))

    // act
    hoursTrack
      .overridePathToArrivalIfRouteIsFaster(hoursTrack(Airport("DUB")),
        Route(Airport("DUB"), Airport("LHR"), 3))

    // assert
    hoursTrack(Airport("LHR")) should
      be(HoursTrackPathValue(Seq(Route(Airport("DUB"),Airport("LHR"), 2))))
  }

  def addReturningRoutes(providedRoutes: Seq[Route]): Seq[Route] = {
    providedRoutes :++ providedRoutes
      .map(currentRoute => Route(currentRoute.arrival, currentRoute.departure, currentRoute.durationHours))
  }
}
