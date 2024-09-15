package midorum.win32.deputy.model;

import java.util.List;
import java.util.Map;

public class Command {

    private final CommandType type;
    private final Map<CommandDataType, String> data;

    public Command(CommandType type, Map<CommandDataType, String> data) {
        this.type = type;
        this.data = data;
    }

    public Command() {
        this.type = null;
        this.data = null;
    }

    public static boolean validateCommandType(final CommandType commandType) {
        return commandType != null;
    }

    public static boolean validateCommandDataEntry(final CommandDataType commandDataType, final String commandDataValue) {
        if (commandDataType == null) return false;
        return commandDataValue != null && !commandDataValue.isBlank();
    }

    public static boolean validateCommandData(final CommandType commandType, final Map<CommandDataType, String> data) {
        final List<List<CommandDataType>> mandatory = commandType.mandatory();
        boolean result = false;
        for (List<CommandDataType> l : mandatory)
            if (l.stream().anyMatch(data::containsKey))
                result = true;
        return result;
    }

    public CommandType getType() {
        return this.type;
    }


    public Map<CommandDataType, String> getData() {
        return this.data;
    }

    @Override
    public String toString() {
        return "{" +
                " type='" + getType() + "'" +
                ", data='" + getData() + "'" +
                "}";
    }

}
