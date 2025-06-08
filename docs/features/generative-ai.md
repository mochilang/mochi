## Generative AI

The `generate` expression invokes large language models. Models can be configured in `model` blocks and then referenced by name. The simplest call passes a text prompt and returns the model's response.

```mochi
let poem = generate text {
  prompt: "Write a haiku about spring"
}
print(poem)
```

Models describe the provider and any default settings. Once declared they can be reused across generate blocks.

```mochi
model quick {
  provider: "openai"
  name: "gpt-3.5-turbo"
  temperature: 0.7
  maxTokens: 200
}

let fancy = generate text {
  model: "quick"
  prompt: "Write a haiku about spring"
}
print(fancy)
```

Generation blocks accept additional fields such as `system` instructions or a `tools` list. Functions can be exposed as tools for the model to call when composing its answer.

```mochi
fun getWeather(location: string): string {
  if location == "Paris" {
    return "sunny with a gentle breeze"
  }
  return "weather data unavailable"
}

let result = generate text {
  prompt: "What's the weather like in Paris?",
  tools: [
    getWeather {
      description: "Returns the current weather for a given city"
    }
  ]
}
print(result)
```

Use `generate embedding` to obtain vector embeddings for search or similarity tasks:

```mochi
let vec = generate embedding {
  text: "hello world",
  normalize: true
}
print(len(vec))
```
