package com.github.riedelcastro.theppl.learn

import com.github.riedelcastro.theppl._
import infer.Argmaxer

/**
 * @author sriedel
 */
trait Evaluator[Context] extends SuperviseByState[Context]{

  var fn = 0
  var tp = 0
  var fp = 0
  var tn = 0
  var numInstances = 0

  def argmaxer(potential:template.PotentialType):Argmaxer

  def reset() {
    fn = 0
    tp = 0
    fp = 0
    tn = 0
    numInstances = 0
  }

  def variables(potential:template.PotentialType) = potential.hidden

  def total = fn + fp + tn + tp
  def totalGold = fn + tp
  def totalGuess = fp + tp
  def prec = nanTo0(tp.toDouble / (tp + fp))
  def recall = nanTo0(tp.toDouble / (tp + fn))
  def f1 = nanTo0(2 * prec * recall / (prec + recall))
  def nanTo0(value: Double) = if (value.isNaN) 0.0 else value

  def probTp = tp / total.toDouble
  def probFp = fp / total.toDouble

  def mcc = nanTo0((tp.toDouble * tn - fp.toDouble * fn) /
    math.sqrt((tp.toDouble + fp) * (tp.toDouble + fn) * (tn.toDouble + fp) * (tn.toDouble + fn)))

  def hook(context:Context, potential:template.PotentialType) {}
  def hook(variable:Variable[Any], gold:Any, guess:Any) {}
  
  def evaluate(instances: Seq[Context]) = {
    reset()
    for (instance <- instances) {
      val potential = template.potential(instance)
      hook(instance,potential)
      val gold = targetState(instance,potential)
      val argmaxer = this.argmaxer(potential)
      val guess = argmaxer.predict
      for (hidden <- variables(potential)) {
        val default = hidden.domain.head
        val goldHidden = gold(hidden)
        val guessHidden = guess(hidden)
        hook(hidden,goldHidden,guessHidden)
        goldHidden -> guessHidden match {
          case (gd, gs) if (gd == gs && gd != default) => tp += 1
          case (gd, gs) if (gd == gs && gd == default) => tn += 1
          case (gd, gs) if (gd != gs && gd != default) => fn += 1
          case (gd, gs) if (gd != gs && gd == default) => fp += 1
        }
      }
      numInstances += 1
    }
    this
  }

  override def toString = {
    """
    | totalGuess: %d
    | totalGold:  %d
    | precision:  %f
    | recall:     %f
    | f1:         %f
    | mcc:        %f""".format(totalGuess, totalGold, prec, recall, f1, mcc).stripMargin
  }

}

