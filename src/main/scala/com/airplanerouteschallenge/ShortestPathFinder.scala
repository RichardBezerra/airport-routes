package com.airplanerouteschallenge

import scala.util.Try

/**
 * ShortestPathFinder define the contract that all solutions must abide by.
 */
trait ShortestPathFinder {

  def findShortestPath(availableRoutes: Seq[Route],
                       departure: Airport,
                       arrival: Airport): Try[Seq[Route]]

  def airports(routes: Seq[Route]): Set[Airport] = {
    routes.flatMap(r => Set(r.departure, r.arrival)).toSet
  }

  def buildGraph(routes: Seq[Route]): Map[Airport, Seq[Route]] = {
    airports(routes).map(dep => dep -> routes.filter(_.departure == dep)).toMap
  }
}

/**
 * Airport defined by its international code.
 * @param iataCode international code.
 */
case class Airport(iataCode: String)

/**
 * A route is defined by an origin, a destination and how long it takes to go from origin to destination.
 * The order of origin and destination matters. A route from A -> B is different from a route from B -> A.
 * @param departure is the origin Airport.
 * @param arrival is the destination Airport.
 * @param durationHours amount of hours between origin and destination.
 */
case class Route(departure: Airport, arrival: Airport, durationHours: Int)

object ExampleRoutes {
  val providedRoutes = Seq(
    Route(Airport("DUB"), Airport("LHR"), 1),
    Route(Airport("DUB"), Airport("CDG"), 2),
    Route(Airport("CDG"), Airport("BOS"), 6),
    Route(Airport("CDG"), Airport("BKK"), 9),
    Route(Airport("ORD"), Airport("LAS"), 2),
    Route(Airport("LHR"), Airport("NYC"), 5),
    Route(Airport("NYC"), Airport("LAS"), 3),
    Route(Airport("BOS"), Airport("LAX"), 4),
    Route(Airport("LHR"), Airport("BKK"), 9),
    Route(Airport("BKK"), Airport("SYD"), 11),
    Route(Airport("LAX"), Airport("LAS"), 2),
    Route(Airport("DUB"), Airport("ORD"), 6),
    Route(Airport("LAX"), Airport("SYD"), 13),
    Route(Airport("LAS"), Airport("SYD"), 14)
  )
}
