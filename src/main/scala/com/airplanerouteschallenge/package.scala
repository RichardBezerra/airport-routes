package com

package object airplanerouteschallenge {
  case object DepartureEqualToArrival extends Throwable
  case object NoRoutesFound extends Throwable
  case object InvalidAirport extends Throwable
}
