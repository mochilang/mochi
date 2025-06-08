## Generative AI

The `generate` expression invokes large language models. Models can be configured in `model` blocks.

```mochi
let poem = generate text {
  prompt: "Write a haiku about spring"
}
print(poem)

model quick {
  provider: "openai"
  name: "gpt-3.5-turbo"
}

let fancy = generate text {
  model: "quick"
  prompt: "Write a haiku about spring"
}
print(fancy)
```

Use functions as tools for the model to call:

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
