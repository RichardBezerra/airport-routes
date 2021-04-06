package com.airplanerouteschallenge

import scala.util.{Failure, Success}

object App {
  def main(args: Array[String]): Unit = {
    val (finder, routes) = args match {
      case Array(_, _, _, "with-returning-routes") => (DirectedCycleGraphFinder, ExampleRoutes.extendedRoutes)
      case _ => (TopologicalOrderingFinder, ExampleRoutes.providedRoutes)
    }

    finder.findShortestPath(routes, Airport(args(1)), Airport(args(2))) match {
      case Success(routes) =>
        routes.foreach(r => println(s"${r.departure} -- ${r.arrival} (${r.durationHours})"))
        println(s"time: ${routes.map(_.durationHours).sum}")

      case Failure(exception) => println(s"Error: $exception")
    }
  }
}
