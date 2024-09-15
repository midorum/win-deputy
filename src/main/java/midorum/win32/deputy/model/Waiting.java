package midorum.win32.deputy.model;

import java.util.List;

/**
 * Прерывание работы сценария, либо пока не выполнятся все проверки либо пока не истечет время
 */
public class Waiting {

    private final String description;
    private final List<Check> checks;
    private final long timeout;

    public Waiting(String description, List<Check> checks, long timeout) {
        this.description = description;
        this.checks = checks;
        this.timeout = timeout;
    }

    public Waiting() {
        this.description = null;
        this.checks = null;
        this.timeout = 0L;
    }

    public static boolean validateCheckListItem(final Check check) {
        return check != null;
    }

    public static boolean validateChecks(final List<Check> checkList) {
        return checkList != null && !checkList.isEmpty();
    }

    public String getDescription() {
        return this.description;
    }

    public List<Check> getChecks() {
        return this.checks;
    }

    public long getTimeout() {
        return this.timeout;
    }

    @Override
    public String toString() {
        return "{" +
            " description='" + getDescription() + "'" +
            ", checks='" + getChecks() + "'" +
            ", timeout='" + getTimeout() + "'" +
            "}";
    }

}
