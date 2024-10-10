package midorum.win32.deputy.model;

import midorum.win32.deputy.common.CommonUtil;

import java.util.List;
import java.util.Map;
import java.util.Optional;

public record Scenario(String title,
                       String description,
                       List<Case> cases,
                       ScenarioType type,
                       Map<ScenarioDataType, String> data) {

    public Scenario {
        if (title == null || title.isBlank()) throw new IllegalArgumentException("The scenario should have the title");
        if (cases == null || cases.isEmpty())
            throw new IllegalArgumentException("The scenario should describe at least one state");
        if (type == null) throw new IllegalArgumentException("The scenario should have the type");
        title = title.trim();
        description = CommonUtil.trimToNull(description);
        cases = List.copyOf(cases);
        data = data != null ? Map.copyOf(data) : Map.of();
    }

    public Optional<String> maybeDescription() {
        return Optional.ofNullable(description);
    }

}
