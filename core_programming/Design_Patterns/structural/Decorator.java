public class Decorator {

  public static void main(String[] args) {
    Program.main(args);
  }

}

// Step 1: Design the component interface

public interface WebPage {
  public void display();
}

// Step 3: Implement the interface with your base concrete component class

public class BasicWebPage implements WebPage {
  private String html = "...";
  private String styleSheet = "...";
  private String scripts = "...";

  public void display() {
    // Renders the HTML to the styleSheet, and run any
    // embedded scripts
    System.out.println("Basic web page");
  }
}

// Step 3: Implement the interface with your abstract decorater class

public abstract class WebPageDecorator implements WebPage {
  protected WebPage page;

  public WebPageDecorator(WebPage webpage) {
    this.page = webpage;
  }

  public void display() {
    this.page.display();
  }
}

// Step 4: Inherit from the abstract decorator and implement
// the component interface with concrete decorator classes

public class AuthorizedWebpage extends WebPageDecorator {

  public AuthorizedWebpage(WebPage decoratedPage) {
    super(decoratedPage);
  }

  public void authorizedUser() {
    System.out.println("Authorizing user");
  }

  public void display() {
    super.display();
    this.authorizedUser();
  }

}

public class AuthenticatedWebPage extends WebPageDecorator {

  public AuthenticatedWebPage(WebPage decoratedPage) {
    super(decoratedPage);
  }

  public void authenticateUser() {
    System.out.println("Authenticating user");
  }

  public void display() {
    super.display();
    this.authenticateUser();
  }

}

public class Program {
  public static void main(String[] args) {

    WebPage myPage = new BasicWebPage();
    myPage = new AuthorizedWebpage(myPage);
    myPage = new AuthenticatedWebPage(myPage);
    myPage.display();
  }
}
