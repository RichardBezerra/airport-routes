package com.airplanerouteschallenge

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
 * Finds the shortest path by applying a topological ordering approach
 * on the graph tha generated from the routes.
 * Then, uses it to find the shortest path from the departure airport.
 *
 * This finder only works when informed routes form a Directed Acyclic Graph (DAG).
 */
object TopologicalOrderingFinder extends ShortestPathFinder {

  case object InvalidDagCyclesFound extends Throwable

  override def findShortestPath(routes: Seq[Route],
                                departure: Airport,
                                arrival: Airport): Try[Seq[Route]] = {

    if (!airports(routes).contains(departure)) {
      return Failure(InvalidAirport(departure.iataCode))
    }

    if (!airports(routes).contains(arrival)) {
      return Failure(InvalidAirport(arrival.iataCode))
    }

    if (arrival == departure) {
      return Failure(DepartureEqualToArrival(departure.iataCode))
    }

    val graph = buildGraph(routes)

    createTopologicalOrder(graph, routes)
      .map(topOrder => findSSSP(graph, topOrder, departure).take(topOrder.indexOf(arrival) + 1).last)
      .map(_._2)
      .filter(_.nonEmpty)
      .orElse(Failure(NoRoutesFound))
  }

  /**
   * Creates a topological ordering representation of the informed graph.
   * @param graph that should be directed and acyclic (DAG).
   * @param routes that were used to build the graph.
   * @return a sequence of airports that represents the topological order
   *         of the graph if the graph is a Directed Acyclic Graph (DAG).
   *         Otherwise, a failure if graph is not a DAG.
   */
  def createTopologicalOrder(graph: Map[Airport, Seq[Route]], routes: Seq[Route]): Try[Seq[Airport]] = {

    // Map[Airport, amount of routes that have this airport as arrival].
    // It could use the graph instead of the routes.
    val initialAmountArrivalConnections: Map[Airport, Int] = airports(routes)
      .map(airport => (airport, routes.count(_.arrival == airport))).toMap

    // A mutable Map[Airport, amount of routes that have this airport as arrival]
    // used to track the the decrement of connections while topological order is being created.
    val mutableAmountArrivalConnections: mutable.Map[Airport, Int] = mutable.Map.from(initialAmountArrivalConnections)

    // Initiate with airports that are not arrival of any route
    val airportsToProcess: mutable.Queue[Airport] =
      mutable.Queue.from(initialAmountArrivalConnections.filter(_._2 == 0).keys)

    var visitedAirportsCounter: Int = 0

    var topologicalOrder: Seq[Airport] = Seq()

    while (airportsToProcess.nonEmpty) {
      val airport = airportsToProcess.dequeue()
      topologicalOrder = topologicalOrder :+ airport
      visitedAirportsCounter = visitedAirportsCounter + 1

      graph.get(airport).foreach(_.foreach { route =>
        // decrease number of routes that are connected the current airport
        // since it was 'removed' from the list of nodes that still needs to be processed.
        mutableAmountArrivalConnections.put(route.arrival, mutableAmountArrivalConnections(route.arrival) - 1)

        // add airport to process list when it no longer have routes to it.
        if (mutableAmountArrivalConnections(route.arrival) == 0) {
          airportsToProcess.enqueue(route.arrival)
        }
      })
    }

    if (visitedAirportsCounter == graph.size) {
      Success(topologicalOrder)
    } else {
      Failure(InvalidDagCyclesFound)
    }
  }

  /**
   * Finds the Single Source Shortest Path (SSSP) from the topological order provided.
   * @param graph that will be used to check the hours in order to build the shortest duration
   *              from the departure to all arrival airports.
   * @param topologicalOrder which sequence will be used to generate the SSSP.
   * @param departure airport. The 'source' of SSSP.
   * @return sequence of tuple as (Airport, Hours distant from the source)
   *         in same order as topological order.
   */
  def findSSSP(graph: Map[Airport, Seq[Route]],
               topologicalOrder: Seq[Airport],
               departure: Airport): Seq[(Airport, Seq[Route])] = {

    // initiate an array to track the hours from source to each airport
    val hoursTracking: mutable.ArraySeq[(Airport, Option[Seq[Route]])] =
      mutable.ArraySeq.from(topologicalOrder.map((_, None)))

    hoursTracking(topologicalOrder.indexOf(departure)) = (departure, Some(Seq()))

    for (airport <- topologicalOrder) {

      val airportIndexAtTopOrder = topologicalOrder.indexOf(airport)

      graph(airport).foreach { route =>

        hoursTracking(airportIndexAtTopOrder)._2.foreach { routesOfCurrentAirport =>
          val arrivalAirportIndexAtTopOrder = topologicalOrder.indexOf(route.arrival)

          // update each airport with minimum duration from the source
          hoursTracking(arrivalAirportIndexAtTopOrder)._2 match {
            case Some(routesAtArrival) =>
              val durationAtCurrentAirport = routesOfCurrentAirport.map(_.durationHours).sum
              val newDuration = durationAtCurrentAirport + route.durationHours
              val durationAtArrival = routesAtArrival.map(_.durationHours).sum

              if (newDuration < durationAtArrival) {
                hoursTracking(arrivalAirportIndexAtTopOrder) = (route.arrival,
                  hoursTracking(airportIndexAtTopOrder)._2.map(_ :+ route))
              }
            case None =>
              hoursTracking(arrivalAirportIndexAtTopOrder) = (route.arrival,
                hoursTracking(airportIndexAtTopOrder)._2.map(_ :+ route))
          }
        }
      }
    }

    hoursTracking.toSeq.map(ht => (ht._1, ht._2.getOrElse(Seq())))
  }
}
