package midorum.win32.deputy.model;

import midorum.win32.deputy.common.CommonUtil;

import java.util.List;

public record Case(long id,
                   String title,
                   String description,
                   List<Check> checks,
                   List<Command> commands,
                   Waiting waiting,
                   boolean repeatable,
                   boolean producesFragileState,
                   boolean ignore) {

    public Case {
        if (title == null || title.isBlank()) throw new IllegalArgumentException("case title cannot be null or empty");
        if (checks == null || checks.isEmpty())
            throw new IllegalArgumentException("case should have at least one check");
        if (commands == null || commands.isEmpty())
            throw new IllegalArgumentException("case should have at least one command");
        title = title.trim();
        description = CommonUtil.trimToNull(description);
        checks = List.copyOf(checks);
        commands = List.copyOf(commands);
    }
}
