public class Document {

  public void deleteText(int position, int length) {
    // ...
  }

  public void insertText(int position, String text) {
    // ...
  }

}

public abstract class Command {
  public abstract void execute();

  public abstract void unexecute();

  public abstract boolean isReversible();
}

public class PasteCommand extends Command {

  private Document document;
  private int position;
  private String text;

  public PasteCommand(Document aDocument, int aPosition, String aText) {
    // ...
  }

  public void execute() {
    document.insertText(position, text);
  }

  public void unexecute() {
    document.deleteText(position, text.length());
  }

  public boolean isReversible() {
    return true;
  }

}

public class CommandManager {
  private static CommandManager instance = null;

  private CommandManager() {
  }

  public static synchronized CommandManager getInstance() {

    if (instance == null)
      instance = new CommandManager();

    return instance;

  }

  public void invokeCommand(Command command) {
    // ...
  }

}

public class Program {

  public static void main(String[] args) {
    Document aDocument = new Document();
    int aPosition = 10;
    String aText = "some string";
    CommandManager commandManager = CommandManager.getInstance();
    Command command = new PasteCommand(aDocument, aPosition, aText);
    commandManager.invokeCommand(command);
  }

}
