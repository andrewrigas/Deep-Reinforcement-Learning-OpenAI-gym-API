package OpenAI.gym

import akka.http.scaladsl.model.{HttpMethod, HttpMethods}

//Super Class
class gymApi {

  //Default Parameters
  val apiRoot: String = "/v1/envs/" //Url Root directory for gym-http-api
  val env: String = "" // Environment
  val envId: String = "" //Environment Instance Id
  val json:String = """{}""" // Empty Json
  val url: String = apiRoot // Final Url default equals with root
  val method: HttpMethod = HttpMethods.POST //Default method POST
}

//Create an instance of the specified environment , Returns: Environment Instance ID
case class createEnv(override val env: String) extends gymApi  {
  override val json = s"""{ "env_id": "${env}" }"""
}

//List all environments running on the server , Returns: List of Environments
case class listEnvs() extends gymApi  {
  override val method = HttpMethods.GET
}

//Reset the state of the environment , Returns: Initial Observation
case class resetEnv(override val envId: String) extends gymApi {
  override val url = s"${apiRoot}${envId}/reset/"
}

//Step though an environment using an action, Returns: {Observation, Reward, Done, info}
case class stepEnv(override val envId: String, action: Int, render: Boolean = true) extends gymApi {
  override val json = s"""{ "action": ${action}, "render": ${render} }"""
  override val url = s"${apiRoot}${envId}/step/"
}

//Get information (name and dimensions/bounds) of the Environment, Returns: info
case class actionSpace(override val envId: String) extends gymApi {
  override val method = HttpMethods.GET
  override val url = s"${apiRoot}${envId}/action_space/"

}

//Get information (name and dimensions/bounds), Returns: info
case class obsSpace(override val envId: String) extends gymApi {
  override val method = HttpMethods.GET
  override val url = s"${apiRoot}${envId}/observation_space/"
}

//Start monitoring
case class monitorStart(override val envId: String, resume: Boolean = false, force: Boolean = false) extends gymApi {
  override val json = s"""{ "resume": ${resume}, "directory": "/openai/tmp", "force": ${force} }"""
  override val url = s"${apiRoot}${envId}/monitor/start/"
}

//Flush all monitor data to disk
case class monitorClose(override val envId: String) extends gymApi {
  override val url = s"${apiRoot}${envId}/monitor/close/"
}

//Flush all monitor data to disk
case class upload(val training_dir: String, val api_key: String,val algorithm_id: String) extends gymApi {
  override val json = s"""{ "training_dir: ${training_dir}, "api_key": ${api_key}, "algorithm_id: ${algorithm_id} }"""
  override val url = "/v1/upload/"
}

//Shut down the Python Server gym-http-api
case class shutdown() extends gymApi  {
  override val url = "/v1/shutdown/"
}