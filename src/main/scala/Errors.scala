object Errors {
  case object DepartureEqualToArrival extends Throwable
  case object NoRoutesFound extends Throwable
  case object InvalidAirport extends Throwable
}
