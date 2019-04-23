package NeuralNetwork


import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions._

import scala.collection.mutable.ListBuffer

class Model {


  //Rename Data types Variables
  type Weight = Double
  type LearningRate = Double

  //Create Data Types
  type Weights = DenseMatrix[Weight]
  type NNLayer = DenseMatrix[Double]


  //Set Learning Rate
  private val learning_rate: LearningRate = 0.0001

  //Create an Empty Model
  private var model: ListBuffer[Layer] = ListBuffer()

  //Initialize Weights List
  private var weights: List[Weights] = List()


  private def getRandomWeightsToLayers(model: List[Layer]): Unit ={

    def getWeights(model: List[Layer]): List[Weights] ={
      model match {
        case Nil => Nil
        case x :: Nil => Nil
        //Get random Weights from the first layer to the output
        case c :: r :: xs => DenseMatrix.rand(r.getNeurons(),c.getNeuronsWithBias(),GaussianDistribution) :: getWeights(model.tail)
      }
    }
    //Store them in our weights variable
    weights = getWeights(model)
  }

  //Get Random Weights from Gaussian Distribution => Standard Deviation 1 and Mean 0
  private def GaussianDistribution = Gaussian(0,1)

  //Add parameters to the model
  def addToModel(dim: Int,bias: Boolean = false,nextLayerActivation:String = "none"): Unit ={
    val layer = Layer(dim,bias,nextLayerActivation)
    this.addToModel(layer)
  }

  //Add layer to the model
  def addToModel(layer: Layer): Unit ={
      model += layer
  }



  def buildModel(): Unit ={
    //Get Last Layer
    val lastLayer = model.last
    //Replace activation layer with end
    val endLayer = Layer(lastLayer.dim,lastLayer.bias,"end")
    //Drop last
    model = model.dropRight(1)
    //Add new layer
    model += endLayer
    //Get Weights in each layer
    getRandomWeightsToLayers(getModel)
  }

  def pretict(state: DenseMatrix[Double]): DenseMatrix[Double] = {
    //Get Prediction and NetworkLayers - Forward Propagation
    val netLayers= forwardProp(state)

    //Return Last Layer
    netLayers.head.activeNet
  }




  def fit(data: DenseMatrix[Double],target: DenseMatrix[Double],epochs: Int):Unit ={

    val immutableWeights = weights

    def train(immutableWeights: List[DenseMatrix[Double]],epochs: Int,counter: Int = 0): List[DenseMatrix[Double]] ={
      if(counter < epochs){
        //Get Prediction and NetworkLayers - Forward Propagation
        val netLayers= forwardProp(data,immutableWeights)

        //Calculate Squared Error
        val sqrError = SquaredError(netLayers.head.activeNet,target)
        //Get Sum off all rows
        val sumSqrError = sum(sqrError(::,*))

        //Gradient Descent Train Weights
        val dW = backProp(netLayers,target,immutableWeights)

        //Store new weights
        val newWeights = for((weights, dw) <- immutableWeights zip dW) yield weights - (learning_rate *:* dw)

        //Call again set counter +1
        train(newWeights,epochs,counter+1)

      }else {
        //Return final trained weights
        immutableWeights
      }

    }
    //Store new Weights
    weights = train(immutableWeights,epochs)

  }


  //Forward Propagation
  private def forwardProp(net: DenseMatrix[Double],weights: List[DenseMatrix[Double]] = weights,network: List[NetworkLayer] = Nil,model: List[Layer] = getModel): List[NetworkLayer] ={
    weights match {
      case Nil => network
      case w :: ws => {
        //Add Bias to the Current Layer Neurons Matrix
        val biasNet = addBiasToMatrix(model.head,net)
        //Matrix Matrix Multiplication with Weights and Current Net
        val nextNet = w * biasNet
        //Activate Neurons
        val activeNet = activation(model.head,nextNet)
        //Create new network Layer
        val newNetLayer = NetworkLayer(biasNet,nextNet,activeNet)
        //Add in the list
        val newNetwork =  newNetLayer :: network
        //Re-call function until weights are null
        forwardProp(activeNet,ws,newNetwork,model.tail)
      }
    }
  }

  private def backProp(network: List[NetworkLayer], target: DenseMatrix[Double], weights: List[DenseMatrix[Double]],model: List[Layer] = getModel)(): List[DenseMatrix[Double]] = {

    def GradientDescent(model: List[Layer],network: List[NetworkLayer],grad: DenseMatrix[Double],weights: List[DenseMatrix[Double]],DW: List[DenseMatrix[Double]]): List[DenseMatrix[Double]] = {
      network match {
        case Nil => DW
        case x => {
          //Check if is the last Layer of NN
          if(model.head.nextLayerActivation == "end"){
            //Get first net layer
            val net = network.head
            //Get last-1 layer
            val currLayer = model.tail.head
            //Calculate the new gradient
            val newGrad = grad *:* getGrad(currLayer,net.activeNet)

            //Calculate the derivative of the first layer
            val dW = newGrad * net.biasNet.t

            //Recall function and remove
            GradientDescent(model.tail,network.tail,newGrad,weights,dW :: DW)

          }  else {

            //Get next layer
            val nextLayer = model.head
            //Current Layer
            val currLayer = model.tail.head
            //Weight of the next layer
            val w = weights.head
            //Actual network layer predictions
            val net = network.head

            //Remove bias weights values if exist
            val wHat = removeWeightsBias(nextLayer,w)

            //Calculate New Derivative
            val newGrad = (wHat.t * grad) *:* getGrad(currLayer,net.activeNet)

            //Calculate derivative of the current layer
            val dW = newGrad * net.biasNet.t

            //Recall function set new grad and pop model ,network and weights last elements
            GradientDescent(model.tail,network.tail,newGrad,weights.tail,dW :: DW)
          }
        }
      }
    }

    //Get derivative of error function
    val grad_error = GradError(network.head.activeNet,target)
    //Reverse Model
    val newModel = getModel.reverse

    //Call Nested Recursive Function  gradientDescent and get all derivatives for all weights
    GradientDescent(newModel,network,grad_error,weights.reverse,Nil)

  }

  private def removeWeightsBias(layer: Layer,w: DenseMatrix[Double]):DenseMatrix[Double] ={
    //Remove last column if the next layer has a bias
    if(layer.bias) removeBiasMatrix(w) else w
  }

  private def getGrad(layer: Layer,net: DenseMatrix[Double]): DenseMatrix[Double] ={
    //Return Derivative of activation function
    layer.nextLayerActivation match {
      case "relu" => GradRelu(net)
      case "sigmoid" => GradSigmoid(net)
      case _ => DenseMatrix.ones(net.rows,net.cols)
    }
  }

  //Derivative of The Error function => Gradient Descent
  private def GradError(prediction: DenseMatrix[Double],target: DenseMatrix[Double]): DenseMatrix[Double] = prediction - target

  //Derivative of The Relu function => Gradient Descent
  private def GradRelu(net: DenseMatrix[Double]): DenseMatrix[Double] = {
    //Create a Matrix with zeros
    val z = DenseMatrix.zeros[Double](net.rows,net.cols)
    val o = DenseMatrix.ones[Double](net.rows,net.cols)
    //Replace each negative value in net Matrix with 0 in grand Matrix
    where(net <:< z,o,z)
  }

  //Derivative of The Sigmoid function => Gradient Descent
  private def GradSigmoid(net: DenseMatrix[Double]): DenseMatrix[Double] = net *:* (1.0 - net)

  private def SquaredError(prediction: DenseMatrix[Double],target: DenseMatrix[Double]): DenseMatrix[Double] ={
    //Calculate the squared cost
    val cost = target -:- prediction
    val sqrCost = pow(cost,2)
    sqrCost /:/ 2.0
  }

  //Add Bias to the Matrix Function
  private def addBiasToMatrix(layer: Layer,data: DenseMatrix[Double]): DenseMatrix[Double] ={
    //Check if that layer has a bias bool true and add add 1.0 to the row
    if(layer.bias)
    {
      val add = DenseMatrix.ones[Double](1,data.cols)
      DenseMatrix.vertcat(data,add)
    }else {
      data
    }
  }

  //Remove last row 0 to -1 => 0 to the end  0 to -2 => 0 to end - 1
  private def removeBiasMatrix(matrix: DenseMatrix[Double]): DenseMatrix[Double] = matrix(::,0 to -2)

  //Check which activation function will activate Neurons
  private def activation(layer: Layer,data: DenseMatrix[Double]): DenseMatrix[Double]={
    layer.nextLayerActivation match {
      case "relu" => reluActivation(data)
      case "sigmoid" => sigmoidActivation(data)
      case _ => data
    }
  }

  //Relu Activation Layer
  private def reluActivation(layer: DenseMatrix[Double]): DenseMatrix[Double] ={
    //Create a Matrix with zeros
      val z = DenseMatrix.zeros[Double](layer.rows,layer.cols)
    //Store every variable that is greater than zero
      where(layer >:> z,layer,z)
  }

  //Sigmoid Activation Layer
  private def sigmoidActivation(layer: DenseMatrix[Double]): DenseMatrix[Double] ={
    sigmoid(layer)
  }



  //Get the model transform it to a list
  def getModel = model.toList

  def getWeights = weights

  //Override method for print
  override def toString: String = (for((layer,i) <- getModel.view.zipWithIndex) yield s"Layer: ${i+1}\n" + layer.toString).mkString("\n")
}

case class NetworkLayer(biasNet: DenseMatrix[Double],nextNet: DenseMatrix[Double],activeNet: DenseMatrix[Double]){
  //Override method for print
  override def toString: String = "BiasNet:\n" + biasNet +"\nNextNet:\n" + nextNet + "\nActiveNet:\n" + activeNet
}

case class Layer(dim: Int,bias: Boolean = false,nextLayerActivation: String = "none"){

  // Get layers without bias
  def getNeurons(): Int= {
    dim
  }

  //Get All Layers with bias
  def getNeuronsWithBias():Int ={
    if(bias){
      dim + 1
    }else {
      dim
    }
  }

  //Override method for print
  override def toString: String = "Neurons: " + dim +" Bias: " + bias + " Activation: " + nextLayerActivation
}