package stringutils

import (
	"regexp"
	"strings"
)

func CleanPhone(phoneNumber string) string {
	re := regexp.MustCompile("[^0-9]")
	return re.ReplaceAllString(phoneNumber, "")
}

func ToSnakeCase(text string) string {
	re := regexp.MustCompile("([A-Z])")
	result := re.ReplaceAllString(text, "_$1")
	result = strings.ToLower(result)
	return strings.TrimPrefix(result, "_")
}

func ToCamelCase(text string) string {
	parts := strings.Split(text, "_")
	for i := 1; i < len(parts); i++ {
		if len(parts[i]) > 0 {
			parts[i] = strings.ToUpper(parts[i][:1]) + parts[i][1:]
		}
	}
	return strings.Join(parts, "")
}

func Truncate(text string, length int, suffix string) string {
	if len(text) <= length {
		return text
	}
	return text[:length-len(suffix)] + suffix
}

func Slugify(text string) string {
	text = strings.ToLower(strings.TrimSpace(text))
	re := regexp.MustCompile("[^\\w\\s-]")
	text = re.ReplaceAllString(text, "")
	re = regexp.MustCompile("[-\\s]+")
	return re.ReplaceAllString(text, "-")
}
