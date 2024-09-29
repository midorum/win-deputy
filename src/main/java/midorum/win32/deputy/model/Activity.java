package midorum.win32.deputy.model;

///import am.ik.yavi.builder.ValidatorBuilder;
///import am.ik.yavi.core.Constraint;
///import am.ik.yavi.core.Validator;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.Optional;

/**
 * Единица активности во время выполнения сценария
 * 
 * @author midorum
 */
public class Activity {

///    public static final Validator<Activity> validator = ValidatorBuilder.<Activity>of()
///            .constraint(Activity::getTitle, "activity title", c -> c.notNull().notBlank())
///            .constraint(Activity::getChecks, "activity checks", c -> c.notNull().notEmpty())
///            .constraint(Activity::getCommands, "activity commands", c -> c.notNull().notEmpty())
///            .forEach(Activity::getChecks, "checks", Check.validator)
////            .forEach(Activity::getCommands, "commands", Command.validator)
///            .build();

    private final String title;
    private final String description;
    private final List<Check> checks;
    private final List<Command> commands;
    private final WaitingList waitingList;
    private final boolean repeatable;
    private final boolean producesFragileState;
    private final boolean ignore;

    public Activity(String title, String description,
                    List<Check> checks,
                    List<Command> commands,
                    WaitingList waitingList,
                    final boolean repeatable,
                    final boolean producesFragileState,
                    final boolean ignore) {
        this.title = title;
        this.description = description;
        this.checks = List.copyOf(checks); // unmodifiable list
        this.commands = List.copyOf(commands); // unmodifiable list
        this.waitingList = waitingList;
        this.repeatable = repeatable;
        this.producesFragileState = producesFragileState;
        this.ignore = ignore;
    }

    public Activity() {
        this.title = null;
        this.description = null;
        this.checks = null;
        this.commands = null;
        this.waitingList = null;
        this.repeatable = false;
        this.producesFragileState = false;
        this.ignore = false;
    }

    public static boolean validateCheckListItem(final Check check) {
        return check != null;
    }

    public static boolean validateChecks(final List<Check> checkList) {
        return checkList != null && !checkList.isEmpty();
    }

    public static boolean validateCommandListItem(final Command command) {
        return command != null;
    }

    public static boolean validateCommands(final List<Command> commandList) {
        return commandList != null && !commandList.isEmpty();
    }

    public String getTitle() {
        return this.title;
    }

    public Optional<String> getDescription() {
        return Optional.ofNullable(this.description);
    }

    public List<Check> getChecks() {
        return this.checks;
    }

    public List<Command> getCommands() {
        return this.commands;
    }

    public Optional<WaitingList> getWaitingList() {
        return Optional.ofNullable(this.waitingList);
    }

    public boolean isRepeatable() {
        return repeatable;
    }

    @JsonProperty("producesFragileState")
    public boolean producesFragileState() {
        return producesFragileState;
    }

    public boolean isIgnore() {
        return ignore;
    }

    @Override
    public String toString() {
        return "Activity{" +
                "title='" + title + '\'' +
                ", description='" + description + '\'' +
                ", checks=" + checks +
                ", commands=" + commands +
                ", waitingList=" + waitingList +
                ", repeatable=" + repeatable +
                ", producesFragileState=" + producesFragileState +
                ", ignore=" + ignore +
                '}';
    }
}
