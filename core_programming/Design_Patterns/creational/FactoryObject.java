// if there are multiple clients that want to instantiate
// the same set of classes, then by using a Factory Object,
// you have cut out redundant code and made software easier to understand
// and to modify

public class KnifeFactory {

  public Knife createKnife(String knifeType) {

    Knife knife = null;

    // concrete instantiation
    if (knifeType.equals("steak")) {

      knife = new SteakKnife();

    } else if (knifeType.equals("chefs")) {

      knife = new ChefsKnife();

    }

  }

}
