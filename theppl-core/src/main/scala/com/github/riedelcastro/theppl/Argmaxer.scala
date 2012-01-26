package com.github.riedelcastro.theppl

/**
 * An object that can calculate the argmax state for a given model.
 * @author sriedel
 */
trait Argmaxer extends HasModel {

  def argmax(penalties: Messages = Messages.empty): ArgmaxResult
  def predict = argmax(Messages.empty).state

}

/**
 * The result of an argmax call. Has the argmaxing state and its score.
 */
trait ArgmaxResult {
  def state: State
  def score: Double
}


trait ArgmaxRecipe[-ModelType <: Model] {
  def argmaxer(model: ModelType, cookbook: ArgmaxRecipe[Model] = DefaultArgmaxers): Argmaxer
}

object Argmaxer {
  def apply(model: Model, cookbook: ArgmaxRecipe[Model] = DefaultArgmaxers) =
    cookbook.argmaxer(model, cookbook)
}
object DefaultArgmaxers extends ArgmaxRecipe[Model] {
  //todo: this should eventually call the default recipe of the model

  def argmaxer(model: Model, cookbook: ArgmaxRecipe[Model]) = {
    model match {
      case f: FiniteSupportModel => BFRecipe.argmaxer(f, cookbook)
      case _ => ArgmaxImpossible.argmaxer(model, cookbook)
    }
  }
  def apply(m: Model) = m match {
    case f: FiniteSupportModel => BFRecipe
    case _ => ArgmaxImpossible
  }
}

object BFRecipe extends ArgmaxRecipe[FiniteSupportModel] {
  def argmaxer(m: FiniteSupportModel, cookbook: ArgmaxRecipe[Model]) =
    new BruteForceArgmaxer {val model = m}
}

object ArgmaxImpossible extends ArgmaxRecipe[Model] {
  def argmaxer(model: Model, cookbook: ArgmaxRecipe[Model]) = sys.error("Argmax impossible")
}


trait BruteForceArgmaxer extends Argmaxer {

  val model: FiniteSupportModel

  def argmax(penalties: Messages) = {
    val states = model.allStates
    def scored = states.map(state => Scored(state, model.penalizedScore(penalties, state)))
    val result = scored.maxBy(_.score)
    new ArgmaxResult {
      def state = result.value
      def score = result.score
    }
  }


}