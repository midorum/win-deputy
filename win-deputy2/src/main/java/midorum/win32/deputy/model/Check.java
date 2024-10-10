package midorum.win32.deputy.model;

import java.util.Map;

public record Check(long id,
                    CheckType type,
                    Map<CheckDataType, String> data,
                    boolean ignore) {

    public Check {
        if (type == null) throw new IllegalArgumentException("check type cannot be null");
        data = data != null ? Map.copyOf(data) : Map.of();
    }

}
