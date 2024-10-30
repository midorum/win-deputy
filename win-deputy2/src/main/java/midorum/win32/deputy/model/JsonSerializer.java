package midorum.win32.deputy.model;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;

import java.io.File;
import java.io.IOException;

public class JsonSerializer<T> {

    private final Class<T> type;
    private final ObjectMapper mapper;

    public JsonSerializer(final Class<T> type) {
        this.type = type;
        final ObjectMapper objectMapper = new ObjectMapper()
                .registerModule(new ParameterNamesModule())
                .registerModule(new Jdk8Module())
                .registerModule(new JavaTimeModule());
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        mapper = objectMapper;
    }

    public String toJson(final T t) throws JsonProcessingException {
        return mapper.writeValueAsString(t);
    }

    public void toFile(final T t, final File file) throws IOException {
        mapper.writeValue(file, t);
    }

    final T fromJson(final String json) throws JsonProcessingException {
        return mapper.readValue(json, type);
    }

    final T fromFile(final File file) throws IOException {
        return mapper.readValue(file, type);
    }

}
