package midorum.win32.deputy.model;

import java.util.Map;


public class Check {

    private final CheckType type;
    private final Map<CheckDataType, String> data;

    public Check(CheckType type, Map<CheckDataType,String> data) {
        this.type = type;
        this.data = data;
    }

    public CheckType getType() {
        return this.type;
    }

    public Map<CheckDataType,String> getData() {
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
