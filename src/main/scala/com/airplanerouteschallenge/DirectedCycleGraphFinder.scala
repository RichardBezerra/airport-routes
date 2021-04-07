package com.airplanerouteschallenge

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

trait DirectedCycleGraphFinder extends ShortestPathFinder {

  override def findShortestPath(availableRoutes: Seq[Route],
                                departure: Airport,
                                arrival: Airport): Try[Seq[Route]] = {

    findShortestPathWith(availableRoutes, departure, arrival)(LazyDijkstra)
  }

  def findShortestPathWith(availableRoutes: Seq[Route],
                           departure: Airport,
                           arrival: Airport): Dijkstra => Try[Seq[Route]] = { finder =>

    val allAirports = airports(availableRoutes)

    if (!allAirports.contains(departure) || !allAirports.contains(arrival)) Failure(InvalidAirport(departure.iataCode))
    else if (departure == arrival) Failure(DepartureEqualToArrival(departure.iataCode))
    else {
      val graph = buildGraph(availableRoutes)

      val hoursDistanceTracking = HoursTrack(allAirports)

      hoursDistanceTracking.setDurationToZero(departure)

      finder.fillHoursTrack(graph, allAirports, departure, arrival, hoursDistanceTracking)

      hoursDistanceTracking
        .get(arrival)
        .filter(_.routes.nonEmpty)
        .map(t => Success(t.routes))
        .getOrElse(Failure(NoRoutesFound))
    }
  }

}

object DirectedCycleGraphFinder extends DirectedCycleGraphFinder

trait Dijkstra {

  def priorityQueue: mutable.PriorityQueue[(Airport, HoursTrackPathValue)]

  def fillHoursTrack(graph: Map[Airport, Seq[Route]],
                     allAirports: Set[Airport],
                     departure: Airport,
                     arrival: Airport,
                     hoursTrack: HoursTrack): Unit
}

object LazyDijkstra extends Dijkstra {

  val priorityQueue: mutable.PriorityQueue[(Airport, HoursTrackPathValue)] =
    mutable.PriorityQueue()(hoursTrackReverseOrdering)

  lazy val hoursTrackReverseOrdering: Ordering[(Airport, HoursTrackPathValue)] = (x: (Airport, HoursTrackPathValue),
                                                                                  y: (Airport, HoursTrackPathValue)) => {
    -x._2.totalDuration.compare(y._2.totalDuration)
  }

  override def fillHoursTrack(graph: Map[Airport, Seq[Route]],
                              allAirports: Set[Airport],
                              departure: Airport,
                              arrival: Airport,
                              hoursTrack: HoursTrack): Unit = {

    priorityQueue.enqueue((departure, HoursTrackPathValue.notInitiated))

    val visitedAirports: mutable.HashMap[Airport, Boolean] = mutable.HashMap.from(allAirports.map((_, false)))

    var arrivalFound = false

    while (priorityQueue.nonEmpty && !arrivalFound) {
      val currentRoute = priorityQueue.dequeue()
      visitedAirports.put(currentRoute._1, true)

      val isDurationToArrivalFaster =
        hoursTrack(currentRoute._1).totalDuration < currentRoute._2.totalDuration

      if (!isDurationToArrivalFaster) {

        graph(currentRoute._1).foreach(route => {
          if (!visitedAirports(route.arrival)) {
            val currentDurationAtDeparture = hoursTrack(currentRoute._1)

            hoursTrack
              .overridePathToArrivalIfRouteIsFaster(currentDurationAtDeparture, route)
              .foreach(priorityQueue.enqueue(_))
          }
        })

        arrivalFound = currentRoute._1 == arrival
      }
    }
  }
}

class HoursTrack extends mutable.HashMap[Airport, HoursTrackPathValue] {
  def setDurationToZero(airport: Airport): Unit = {
    this.put(airport, HoursTrackPathValue())
  }

  def overridePathToArrivalIfRouteIsFaster(currentTracking: HoursTrackPathValue,
                                           route: Route): Option[(Airport, HoursTrackPathValue)] = {
    this (route.arrival) match {
      case HoursTrackPathValue.notInitiated =>
        val firstTrackingPath = HoursTrackPathValue(currentTracking.routes :+ route)
        this.put(route.arrival, firstTrackingPath)
        Some((route.arrival, firstTrackingPath))

      case arrivalTracking : HoursTrackPathValue =>
        currentTracking match {
          case tracking @ HoursTrackPathValue(routes)
            if tracking.totalDuration + route.durationHours < arrivalTracking.totalDuration =>
            val fasterTrackingPath = HoursTrackPathValue(routes :+ route)
            this.put(route.arrival, fasterTrackingPath)
            Some((route.arrival, fasterTrackingPath))

          case _ => None
        }
    }
  }
}

object HoursTrack {
  def apply(airports: Set[Airport]): HoursTrack = {
    new HoursTrack().addAll(airports.map((_, HoursTrackPathValue.notInitiated)))
  }
}

case class HoursTrackPathValue(routes: Seq[Route]) {
  val totalDuration: Int = routes.map(_.durationHours).sum
}

object HoursTrackPathValue {
  def apply(): HoursTrackPathValue = new HoursTrackPathValue(Seq())

  def apply(routes: Seq[Route]): HoursTrackPathValue = new HoursTrackPathValue(routes)

  val notInitiated: HoursTrackPathValue = new HoursTrackPathValue(Seq())
}

