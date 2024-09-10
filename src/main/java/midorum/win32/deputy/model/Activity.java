package midorum.win32.deputy.model;

import java.util.List;
import java.util.Optional;

/**
 * Единица активности во время выполнения сценария
 * 
 * @author midorum
 */
public class Activity {

    private final String title;
    private final String description;
    private final List<Check> checks;
    private final List<Command> commands;
    private final Waiting waiting;

    public Activity(String title, String description, List<Check> checks, List<Command> commands, Waiting waiting) {
        this.title = title;
        this.description = description;
        this.checks = List.copyOf(checks); // unmodifiable list
        this.commands = List.copyOf(commands); // unmodifiable list
        this.waiting = waiting;
    }

    public String getTitle() {
        return this.title;
    }

    public String getDescription() {
        return this.description;
    }

    public List<Check> getChecks() {
        return this.checks;
    }

    public List<Command> getCommands() {
        return this.commands;
    }

    public Optional<Waiting> getWaiting() {
        return Optional.ofNullable(this.waiting);
    }

    @Override
    public String toString() {
        return "{" +
            " title='" + getTitle() + "'" +
            ", description='" + getDescription() + "'" +
            ", checks='" + getChecks() + "'" +
            ", commands='" + getCommands() + "'" +
            "}";
    }

}
