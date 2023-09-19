import java.util.ArrayList;

public class Subject {

  private ArrayList<Observer> observers = new ArrayList<Observer>();

  public void registerObserver(Observer observer) {
    observers.add(observer);
  }

  public void notifyObservers() {
    for (Observer o : observers) {
      o.update();
    }
  }
}

public class Blog extends Subject {
  private String state;

  public String getState() {
    return state;
  }

  // blog responsibilities
}

public interface Observer {

  public void update();

}
