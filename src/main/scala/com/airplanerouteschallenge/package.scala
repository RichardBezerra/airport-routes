package com

package object airplanerouteschallenge {
  case class DepartureEqualToArrival(iata: String) extends Throwable(s"departure and arrival airports are the same: $iata")
  case object NoRoutesFound extends Throwable("No path found from departure to arrival informed")
  case class InvalidAirport(invalidIata: String) extends Throwable(s"$invalidIata is not in the list of valid airports")
}
