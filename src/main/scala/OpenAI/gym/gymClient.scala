package OpenAI.gym


import akka.actor.{ActorSystem, Terminated}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpEntity, _}
import akka.http.scaladsl.unmarshalling._
import akka.stream.ActorMaterializer

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}

object gymClient {

  //Actor represented by an ActorRef which is basically a pointer
  //It has an Ordered mailbox with messages
  //Processing asynchronously immutable messages
  //One message handled at a time
  //Each actor has a unique addresses
  //They dont have shared state
  //It can Change behaviour at runtime

  private implicit val system = ActorSystem.create("system") // Create an Actor System for messages communication

  implicit def materializer: ActorMaterializer = ActorMaterializer()

  val executionContext: ExecutionContextExecutor = system.dispatcher //Implicit dispatcher for future function calls

  private val host: String = "http://127.0.0.1" // Local Host address
  private val port: Int = 5000 // Port Number
  val timeout: Int = 20 // Timeout of the request

  private val contentType = ContentTypes.`application/json` //Data Content Type

  def terminate: Future[Terminated] = system.terminate() // Terminate Actor System

  def requestToApi(request: gymApi): HttpResponse = {
    val url = host + ":" + port + request.url
    val httpRequest = HttpRequest(uri = url).withMethod(request.method).withEntity(HttpEntity(contentType, request.json))
    val responseFuture: Future[HttpResponse] = Http().singleRequest(httpRequest)
    Await.result(responseFuture, timeout.second)
  }



}
