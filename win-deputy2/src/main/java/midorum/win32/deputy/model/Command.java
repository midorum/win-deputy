package midorum.win32.deputy.model;

import java.util.Map;

public record Command(CommandType type,
                      Long checkId,
                      Map<CommandDataType, String> data) {

    public Command {
        if (type == null) throw new IllegalArgumentException("command type cannot be null");
        data = data != null ? Map.copyOf(data) : Map.of();
    }
}
