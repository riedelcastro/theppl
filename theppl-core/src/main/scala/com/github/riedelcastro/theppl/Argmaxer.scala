package com.github.riedelcastro.theppl

/**
 * @author sriedel
 */
trait Argmaxer {

  val model: Model

  def argmax(penalties: Message): ArgmaxResult

}

trait ArgmaxRecipe[-ModelType <: Model] {
  def argmaxer(model: ModelType, cookbook: ArgmaxRecipe[Model] = ArgmaxCookbook): Argmaxer
}

object ArgmaxCookbook extends ArgmaxRecipe[Model] {
  //todo: this should eventually call the default recipe of the model

  def argmaxer(model: Model, cookbook: ArgmaxRecipe[Model]) = {
    model match {
      case f:FiniteSupportModel => BFRecipe.argmaxer(f,cookbook)
      case _ => ArgmaxImpossible.argmaxer(model,cookbook)
    }
  }
  def apply(m: Model) = m match {
    case f:FiniteSupportModel => BFRecipe
    case _ => ArgmaxImpossible
  }
}

object BFRecipe extends ArgmaxRecipe[FiniteSupportModel] {
  def argmaxer(m: FiniteSupportModel, cookbook: ArgmaxRecipe[Model]) =
    new BFArgmaxer {val model = m}
}

object ArgmaxImpossible extends ArgmaxRecipe[Model] {
  def argmaxer(model: Model, cookbook: ArgmaxRecipe[Model]) = sys.error("Argmax impossible")
}


trait BFArgmaxer extends Argmaxer {

  val model: FiniteSupportModel

  def argmax(penalties: Message) = {
    val states = model.allStates
    def scored = states.map(state => Scored(state, model.penalizedScore(penalties, state)))
    val result = scored.maxBy(_.score)
    new ArgmaxResult {
      def state = result.value
      def score = result.score
    }
  }


}