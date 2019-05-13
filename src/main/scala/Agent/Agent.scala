package Agent
import NeuralNetwork.Model
import OpenAI.gym._
import breeze.linalg._


import scala.util.Random

case class Agent(env: Environment){

  //Get State Size
  val states = env.observation_space().info.shape

  //Get Action Size
  val action_size = env.action_space().info.n

  //Calculate State_size
  def state_size: Int = states.head

  //Epsilon to explore
  var epsilon = 100.0
  //Epsilon Min to explore
  val epsilonMin = 10.0
  //Decrease epsilon value
  val epsilonDecay: Double = 0.1
  //Discount factor
  val gamma = 0.9
  //Size of train data
  val batch_size = 32
  //Max Memory of states to remember
  val max_memory_size = 1000


  def runDeepQN(trials: Int,episodes: Int)= {
    //Build new Model
    val model = buildModel()
    //run trials
    this.trials(model,trials,episodes,Nil)
  }

  private def trials(model: Model,trials: Int,episodes: Int,memory: List[Experience]): Model ={
    if(trials > 0){
      //Reset Environment
      val observation = env.reset()
      //save observation to DenseMatrix
      val currState = toDenseMatrix(observation.observation)
      //Run episodes and get new trained model and newMemory
      val (newModel,newMemory) = this.episodes(model,currState,episodes,memory)
      //Decrease epsilon
      if(epsilon > epsilonMin) epsilon -= epsilonDecay
      //Call trials Again
      this.trials(newModel,trials-1,episodes,newMemory)
    }else {
      model
    }
  }

  private def episodes(model: Model,currState: DenseMatrix[Double],episodes: Int,memory: List[Experience],done: Boolean = false): (Model,List[Experience]) ={
    if(!done && episodes > 0){
      //Take an E-greedy action
      val action = E_greedy(currState,model)
      //Make a step and get new observation
      val stepReply = env.step(action)
      //Make DenseMatrix from observation
      val nextState = toDenseMatrix(stepReply.observation)
      //Get store new experience and get new Memory
      val newMemory = storeExp(memory,currState,action,stepReply.reward,nextState,stepReply.done)
      //Train model
      val newModel = if(newMemory.length > batch_size) train(model,newMemory) else model
      //Recall function
      this.episodes(newModel,nextState,episodes-1,newMemory,stepReply.done)
    }else {
      //Return a tuple
      (model,memory)
    }
  }

  private def storeExp(memory: List[Experience],currState: DenseMatrix[Double],action: Int,reward: Double,nextSate: DenseMatrix[Double], done: Boolean): List[Experience] ={
    val exp = Experience(currState,action,reward,nextSate,done)
    //Check if we store the maximum size o memory
    if(max_memory_size >= memory.size){
      //store at the end the new experience
      memory :+ exp
    }else {
      //drop from begin
      val newMemory = memory.drop(1)
      //add to last
      newMemory :+ exp
    }
  }

  //Q-learning strategy
  private def E_greedy(currState: DenseMatrix[Double],model: Model): Int = {

    if(Random.nextInt(100) <= epsilon){
      //Explore take random Action
      env.action_space().sample()
    }else {
      //Get max index value from DenseMatrix prediction
      argmax(model.pretict(currState))._1
    }
  }

  //Seq[Double] to DenseMatrix
  private def toDenseMatrix(data: Seq[Double]): DenseMatrix[Double] = DenseMatrix.create(state_size,1,data.toArray)

  def buildModel(): Model = {
    val model = new Model()
    //Input Layer Relu Activation
    model.addToModel(state_size,true,"relu")
    //First Hidden Layer Relu activation
    model.addToModel(FirstHiddenLayer,true,"relu")
    //Second Hidden Layer Linear Activation
    model.addToModel(SecondHiddenLayer,true,"linear")
    //Output Layer
    model.addToModel(action_size)
    //Build Model
    model.buildModel()
    //Return Model
    model
  }

  private def train(model: Model,memory: List[Experience]): Model ={

    //Select Randomly a batch of memory experiences
    val batch = Random.shuffle(memory).take(batch_size)

    for(experience <- batch){
      //Transform observations to matrix
      val nextState = experience.nextSate
      val currState = experience.currState

      // target value = reward + (discount rate gamma) * (maximum target Q based on future action a')
      val newTarget = if(!experience.done) experience.reward + gamma * max(model.pretict(nextState)) else experience.reward

      //Get NN prediction
      val prediction = model.pretict(currState)
      //Update matrix value to the new Target value
      prediction(experience.action,0) = newTarget
      //Store prediction in target
      val target =  prediction
      //10 epoch of training with data: currState and target prediction with update value
      model.fit(currState,target,epochs = 10)
    }
    model
  }

  //The Optimal Number of first and second hidden nodes by Huang
  //Where I = input nodes
  //O = output nodes
  //First Hidden Layer sqrt[(O+2)I] + 2sqrt[I/(O+2)]
  private def FirstHiddenLayer = math.round(math.sqrt((action_size+2)*state_size) + 2 * math.sqrt(state_size/(action_size+2))).toInt
  //Second Hidden Layer O*sqrt[I/(O+2)
  private def SecondHiddenLayer= math.round( action_size * math.sqrt(state_size/(0+2))).toInt


}

//Experience
case class Experience(currState: DenseMatrix[Double],action: Int,reward: Double,nextSate: DenseMatrix[Double], done: Boolean)