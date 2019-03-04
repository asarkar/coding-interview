package org.asarkar.codinginterview.design

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class AutocompleteSpec extends FlatSpec {
  "autocomplete" should "return suggestions" in {
    val autocomplete = new Autocomplete(Seq("dog", "deer", "deal"))
    autocomplete.suggestions("de") should contain only("deer", "deal")
    autocomplete.suggestions("do") should contain only "dog"
  }
}
