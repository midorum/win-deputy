package midorum.win32.deputy.model;

import java.util.Map;

public class Command {

private final CommandType type;
private final Map<CommandDataType, String> data;

    public Command(CommandType type, Map<CommandDataType,String> data) {
        this.type = type;
        this.data = data;
    }

    public CommandType getType() {
        return this.type;
    }


    public Map<CommandDataType,String> getData() {
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
