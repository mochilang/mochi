package build

import "fmt"

// generateCsproj returns .csproj XML for a single-file Mochi app.
// runtimeCsproj is the absolute path to Mochi.Runtime.csproj.
func generateCsproj(className, tfm, runtimeCsproj string) string {
	projRef := ""
	if runtimeCsproj != "" {
		projRef = fmt.Sprintf(`
  <ItemGroup>
    <ProjectReference Include="%s" />
  </ItemGroup>`, runtimeCsproj)
	}
	return fmt.Sprintf(`<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>%s</TargetFramework>
    <AssemblyName>%s</AssemblyName>
    <Nullable>enable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
    <LangVersion>latest</LangVersion>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
    <Deterministic>true</Deterministic>
    <PathMap>$(MSBuildProjectDirectory)=/_/</PathMap>
    <DebugType>none</DebugType>
    <Optimize>true</Optimize>
  </PropertyGroup>%s
</Project>
`, tfm, className, projRef)
}
