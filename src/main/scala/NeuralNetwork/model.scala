package NeuralNetwork



class model(var nn: List[Layer]) {

  private implicit var learning_rate = 0.001

  var Weight = Array()

  var Weights = Seq(Weight)


  def addLayers(layer: Layer) ={
    if(nn.size <= 3){
      nn = nn :+ layer
    } else {
      println("Reach Maximum size of layers in model : 3 layers registered => input-hidden-output")
    }

  }

  def giveWeightsDim() ={

  }

  def buildNeuralNetwork() ={

  }

}


class Layer(dim: Int,bias: Boolean = true)