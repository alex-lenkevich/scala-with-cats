package app

import cats.data.{EitherT, StateT}
import cats.mtl.MonadState
import cats.{Monad, MonadError}
import monix.eval.Task
import monix.execution.Scheduler
import app.Config._
import app.Main._
import cats.syntax.all._
import cats.instances.tuple._

import scala.language.higherKinds

object Main extends Program {

  import mtl._
  import cats.implicits._
  import cats.mtl.implicits._

  def main(args: Array[String]): Unit = {

    val config = Config("localhost", 8080)
    val requests = Requests.empty

    type Effect0[A] = EitherT[Task, Error, A]
    type Effect[A] = StateT[Effect0, Requests, A]

    implicit val configAsk = constantAsk[Effect, Config](config)
    implicit val consoleEffect = Console.console[Effect]
    implicit val weather = Weather.weather[Effect](config)

    val app: Effect[Unit] = program[Effect]

    implicit val io = Scheduler.io("io-scheduler")

    (app.run(requests).value >>= {
      case Left(error) =>
        Console.console[Task].printLn(s"Encountered an error: $error")
      case Right(_) =>
        ().pure[Task]
    }).runSyncUnsafe()
  }
}

trait Program extends Effects {

  def cityByName[F[_] : ErrorHandler](cityName: String): F[City] =
    cityName match {
      case "Wroclaw" => City(cityName).pure
      case "Cadiz" => City(cityName).pure
      case _ => UnknownCity(cityName).raiseError
    }

  def hottestCity[F[_] : RequestsState]: F[(City, Temperature)] =
    requestState.inspect(reqs =>
      Requests.hottest(reqs).map(_.temperature)
    )


  def askCity[F[_] : Console : Monad]: F[String] =
    for {
      _ <- console.printLn("What is the next city?")
      cityName <- console.readLn
    } yield cityName

  def fetchForecast[F[_] : Weather : RequestsState : Monad](city: City)(implicit state: RequestsState[F]): F[Forecast] =
    for {
      maybeForecast <- state.inspect(_.get(city))
      forecast <- maybeForecast.fold(weather.forecast(city))(_.pure)
      _ <- state.modify(_ + (city -> forecast))
    } yield forecast

  def askFetchJudge[F[_] : Console : Weather : RequestsState : ErrorHandler]: F[Unit] =
    for {
      cityName <- askCity
      city <- cityByName(cityName)
      forecast <- fetchForecast(city)
      _ <- console.printLn(s"Forecast for $city is ${forecast.temperature}")
      hottest <- hottestCity
      _ <- console.printLn(s"Hottest city found so far is $hottest")
    } yield ()

  def program[F[_] : ConfigAsk : Console : Weather : RequestsState : ErrorHandler]: F[Unit] =
    for {
      h <- host
      p <- port
      _ <- console.printLn(s"Using weather service at http://$h:$p")
      _ <- askFetchJudge.foreverM[Unit]
    } yield ()
}

trait Effects {

  type ErrorHandler[F[_]] = MonadError[F, Error]
  type RequestsState[F[_]] = MonadState[F, Requests]

  def requestState[F[_]](implicit requestsState: RequestsState[F]) = requestsState
  def console[F[_]](implicit console: Console[F]) = console
  def weather[F[_]](implicit weather: Weather[F]) = weather
}
