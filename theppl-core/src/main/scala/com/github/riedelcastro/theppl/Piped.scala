package com.github.riedelcastro.theppl

/**
 * @author sriedel
 */
//trait Piped extends Module {
//  self =>
//
//  type Inner <: Variable[_]
//
//  type Module1 <: Module {type Context = self.Context; type Hidden = Inner}
//  type Module2 <: Module {type Context = self.Context; type Observed = Inner}
//
//  val module1: Module1
//  val module2: Module2
//
//  type Hidden = module2.Hidden
//  type Observed = module1.Observed
//  type ModelType <: PipedModel
//
//  abstract class PipedModel(val model1: module1.ModelType, val model2: module2.ModelType, val argmax1: State) extends Model {
//    val context = model1.context
//    val hidden = model2.hidden
//    val observed = model1.observed
//    val observation = model1.observation
//    lazy val score1 = model1.score(argmax1)
//    def argmax(penalties: Message) = model2.argmax(penalties)
//  }
//
//  def createPipedModel(model1: module1.ModelType, model2: module2.ModelType, argmax1: State): ModelType
//
//  def model(context: Context, observation: State) = {
//    val model1 = module1.model(context, observation)
//    val argmax1 = model1.argmax(null)
//    val model2 = module2.model(context, argmax1)
//    createPipedModel(model1, model2, argmax1)
//  }
//}

//class PipedSimple[C, I <: Variable[_]](mod1: Module {type Context = C; type Hidden = I},
//                                       mod2: Module {type Context = C; type Observed = I})
//  extends Piped with NoSerialization {
//  type Context = C
//  type Inner = I
//  type Module1 = Module {type Context = C; type Hidden = I}
//  type Module2 = Module {type Context = C; type Observed = I}
//  val module1 = mod1
//  val module2 = mod2
//  type ModelType = PipedModel
//  def createPipedModel(model1: module1.ModelType, model2: module2.ModelType, argmax1: State): ModelType = {
//    new PipedModel(model1, model2, argmax1){
//      def score(state: State) = score1 + model2.score(state)
//    }
//  }
//}

//class PipedLinear[C, I <: Variable[_]](mod1: Module {type Context = C; type Hidden = I},
//                                       mod2: LinearModule {type Context = C; type Observed = I})
//  extends Piped with LinearModule with NoSerialization {
//  type Context = C
//  type Inner = I
//  type Module1 = Module {type Context = C; type Hidden = I}
//  type Module2 = LinearModule {type Context = C; type Observed = I}
//  val module1 = mod1
//  val module2 = mod2
//
//  def weights = module2.weights
//  type ModelType = PipedLinearModel
//  class PipedLinearModel(model1: module1.ModelType, model2: module2.ModelType, argmax1: State)
//    extends PipedModel(model1, model2, argmax1) with LinearModel {
//
//    override def score(state: State) = score1 + super.score(state)
//    def features(state: State) = model2.features(state)
//    override def featureDelta(gold: State, guess: State) = model2.featureDelta(gold, guess)
//  }
//
//  def createPipedModel(model1: module1.ModelType, model2: module2.ModelType, argmax1: State): ModelType = {
//    new PipedLinearModel(model1, model2, argmax1)
//  }
//}

