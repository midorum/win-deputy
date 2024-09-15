package midorum.win32.deputy.model;

///import am.ik.yavi.builder.ValidatorBuilder;
///import am.ik.yavi.core.Constraint;
///import am.ik.yavi.core.Validator;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Именованный список активностей
 *
 * @author midorum
 */
public class Scenario {

///     public static final Validator<Scenario> validator = ValidatorBuilder.<Scenario>of()
///            .constraint(Scenario::getTitle, "scenario title", c -> c.notNull().notBlank())
///            .constraint(Scenario::getActivities, "scenario activities", c -> c.notNull().notEmpty())
///            .constraint(Scenario::getType, "scenario type", Constraint::notNull)
///            .forEach(Scenario::getActivities, "scenario activities", Activity.validator)
///            .build();

    private final String title;
    private final String description;
    private final List<Activity> activities;
    private final ScenarioType type;
    private final Map<ScenarioDataType, String> data;

    public Scenario(String title, String description, List<Activity> activities, ScenarioType type, Map<ScenarioDataType, String> data) {
        this.title = title;
        this.description = description;
        this.activities = activities;
        this.type = type;
        this.data = data;
    }

    public Scenario() {
        this.title = null;
        this.description = null;
        this.activities = null;
        this.type = null;
        this.data = null;
    }

    public static boolean validateActivityListItem(final Activity activity) {
        return activity != null;
    }

    public static boolean validateActivities(final List<Activity> activitiesList) {
        return activitiesList != null && !activitiesList.isEmpty();
    }

    public String getTitle() {
        return this.title;
    }

    public Optional<String> getDescription() {
        return Optional.ofNullable(this.description);
    }

    public List<Activity> getActivities() {
        return this.activities;
    }

    public ScenarioType getType() {
        return this.type;
    }

    public Optional<Map<ScenarioDataType, String>> getData() {
        return Optional.ofNullable(this.data);
    }

    @Override
    public String toString() {
        return "{" +
                " title='" + getTitle() + "'" +
                ", description='" + getDescription() + "'" +
                ", activities='" + getActivities() + "'" +
                ", type='" + getType() + "'" +
                ", data='" + getData() + "'" +
                "}";
    }


}
