package NeuralNetwork

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions._
import org.apache.commons.io.filefilter.FalseFileFilter

import scala.collection.mutable.ListBuffer

class Model {

  //Rename Data types Variables
  type Weight = Double
  type LearningRate = Double

  //Create Data Types
  type Weights = DenseMatrix[Weight]
  type NNLayer = DenseMatrix[Double]


  //Set Learning Rate
  private val learning_rate: LearningRate = 0.001

  //Create an Empty Model
  private var model: ListBuffer[Layer] = ListBuffer()

  //Initialize Weights List
  private var weights: List[Weights] = List()

  private def getRandomWeightsToLayers(model: List[Layer]): Unit = {

    def getWeights(model: List[Layer]): List[Weights] ={
      model match {
        case Nil => Nil
        case x :: Nil => Nil
          //Get random Weights from the first layer to the output
        case c :: r :: xs => DenseMatrix.rand(r.getNeurons(),c.getNeuronsWithBias(),GaussianDistribution)  :: getWeights(model.tail)
      }
    }

    //Store them in our weights variable
    weights = getWeights(model)
  }

  //Get Random Weights from Gaussian Distribution => Standard Deviation 1 and Mean 0
  private def GaussianDistribution = Gaussian(0,1)

  //Add parameters to the model
  def addToModel(dim: Int,bias: Boolean = false,nextLayerActivation:String = "none"): Unit ={
    val layer = new Layer(dim,bias,nextLayerActivation)
    this.addToModel(layer)
  }

  //Add layer to the model
  def addToModel(layer: Layer): Unit ={

    if(model.length == 2){
      //Add Last Layer
      model += layer

      //On The 3rd layer get Random Weights
      getRandomWeightsToLayers(model.toList)

    }else if(model.length < 2){

      //Add layer
      model += layer

    } else{

      println("Supported layers : 3")
    }
  }

  def fit(data: DenseMatrix[Double],target: DenseMatrix[Double]):Unit ={
    //Forward-Propagation
    val immutableWeights = weights

    //Get Prediction - Forward Propagation
    val prediction = forwardProp(getModel,data,immutableWeights)

    val sumSquaredError = sum(SquaredError(prediction,target)(::,*))

  }

  //Forward Propagation
  private def forwardProp(model: List[Layer],net: DenseMatrix[Double],weights: List[Weights]): DenseMatrix[Double] ={
    weights match {
      case Nil => net
      case w :: ws => {
        //Add Bias to the Current Layer Neurons Matrix
        val newNet = addBiasToMatrix(model.head,net)
        //Matrix Matrix Multiplication with Weights and Current Net
        val nextNet = w * newNet
        //Activate Neurons
        val activeNet = activation(model.head,nextNet)
        //Re-call function until weights are null
        forwardProp(model.tail,activeNet,ws)
      }
    }
  }

  private def backProp() ={

  }

  private def SquaredError(prediction: DenseMatrix[Double],target: DenseMatrix[Double]): DenseMatrix[Double] = {
    //Calculate the squared cost
    val cost = target -:- prediction
    val sqrCost = pow(cost,2)
    sqrCost /:/ 2.0
  }
  //Add Bias to the Matrix Function
  private def addBiasToMatrix(layer: Layer,data: DenseMatrix[Double]): DenseMatrix[Double] = {
    //Check if that layer has a bias bool true and add add 1.0 to the row
    if(layer.getBias())
    {
      val add = DenseMatrix.ones[Double](1,data.cols)
      DenseMatrix.vertcat(data,add)
    }else {
      data
    }
  }

  //Check which activation function will activate Neurons
  private def activation(layer: Layer,data: DenseMatrix[Double]): DenseMatrix[Double] = {
    layer.getActivation() match {
      case "relu" => reluActivation(data)
      case "sigmoid" => sigmoidActivation(data)
      case _ => data
    }
  }

  //Relu Activation Layer
  private def reluActivation(layer: NNLayer): NNLayer={
    //Create a Matrix with zeros
      val z = DenseMatrix.zeros[Double](layer.rows,layer.cols)
    //Store every variable that is greater than zero
      where(layer >:> z,layer,z)
  }

  //Sigmoid Activation Layer
  private def sigmoidActivation(layer: NNLayer): NNLayer ={
    sigmoid(layer)
  }

//  def softMaxActivation(layer: NNLayer): NNLayer ={
//    softmax.apply(layer)
//  }

  //Get the model transform it to a list
  def getModel = model.toList

  //Override method for print
  override def toString: String = (for((layer,i) <- getModel.view.zipWithIndex) yield s"Layer: ${i+1}\n" + layer.toString).mkString("\n")
}


class Layer(dim: Int,bias: Boolean = false,nextLayerActivation: String = "none"){

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

  //Return Bool for Bias
  def getBias() ={
    bias
  }

  //Return Activation Method
  def getActivation():String = {
    nextLayerActivation
  }

  //Override method for print
  override def toString: String = "Neurons: " + dim +" Bias: " + bias + " Activation: " + nextLayerActivation
}