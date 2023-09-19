// The adapter pattern convert the interface of a third-party class into another
// interface clients expect.Adapter lets classes work together that couldn’ t
// otherwise because of incompatible interfaces and also helps swapping third-party libraries easier

// Following example is converts an object to json object

// Design the target interface
public interface WebRequester {

  public int request(Object);

}

// Implement the target interface with the adapter class
public class WebAdapter implements WebRequester {
  private WebService service;

  public void connect(WebService currentService) {
    this.service = currentService;
  }

  public int request(Object request) {
    Json result = this.toJson(request);
    Json response = service.request(result);
    if (response != null)
      return 200; // OK status code
    return 500; // Server error status code
  }

  private Json toJson(Object input) {
    // ...
  }

}

// Send the request from the client to the adapter using the target interface
public class WebClient {
  private WebRequester webRequester;

  public WebClient(WebRequester webRequester) {
    this.webRequester = webRequester;
  }

  private Object makeObject() {
    // Make an Object
  }

  public void doWork() {
    Object object = makeObject();
    int status = webRequester.request(object);
    if (status == 200) {
      System.out.println("OK");
    } else {
      System.out.println("Not OK");
    }
    return;
  }
}

// Main program
public class Program {

  public static void main(String[] args) {
    WebService service = new WebService(webHost);
    WebAdapter adapter = new WebAdapter();
    adapter.connect(service);
    WebClient client = new WebClient(adapter);
    client.doWork();
  }

}
