public abstract class PastaDish {

  public final void makeRecipe() {
    boilWater();
    addPasta();
    cookPasta();
    drainAndPlate();
    addSauce();
    addProtein();
    addGarnish();
  }

  private void boilWater() {
    // ...
  }

  private void drainAndPlate() {
    // ...
  }

  private void cookPasta() {
    // ...
  }

  protected abstract void addPasta();

  protected abstract void addSauce();

  protected abstract void addProtein();

  protected abstract void addGarnish();

}

public class SpaghettiMeatballs extends PastaDish {
  protected void addPasta() {
    System.out.println("Add spaghetti");
  }

  protected void addProtein() {
    System.out.println("Add meatballs");
  }

  protected void addSauce() {
    System.out.println("Add tomato sauce");
  }

  protected void addGarnish() {
    System.out.println("Add Parmesan sauce");
  }

}

public class PenneAlfredo extends PastaDish {
  protected void addPasta() {
    System.out.println("Add penne");
  }

  protected void addProtein() {
    System.out.println("Add chicken");
  }

  protected void addSauce() {
    System.out.println("Add Alfredo sauce");
  }

  protected void addGarnish() {
    System.out.println("Add parsley");
  }

}
