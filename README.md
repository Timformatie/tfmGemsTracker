# tfmGemsTracker

An R package providing utility functions for retrieving and working with GemsTracker data via the Pulse API.

## Description

`tfmGemsTracker` simplifies interaction with GemsTracker systems by providing a set of functions to:
- Authenticate with the Pulse API
- Retrieve patient, careplan, and task information
- Query questionnaire responses
- Manage activity logs
- Handle URL encoding/decoding for secure data exchange

## Dependencies

This project uses `renv` to manage package dependencies and ensure reproducibility across different environments.

### First-Time Setup

When you first clone this repository, run:

```r
# Install renv if not already installed
install.packages("renv")

# Restore the project library from the lockfile
renv::restore()
```

This will install all the required packages with the exact versions specified in the `renv.lock` file.

## Environment Variables

This package requires several environment variables to be set for API authentication and configuration.

### Required Variables

Create a `.Renviron` file in your project root or home directory with the following variables:

```bash
# Base URL for the Pulse API
PULSE_BASE_URL="https://your-pulse-instance.com/"

# API Credentials
PULSE_API_CLIENT_ID="your_client_id"
PULSE_API_CLIENT_SECRET="your_client_secret"

# User Credentials
PULSE_USERNAME="your_username"
PULSE_PASSWORD="your_password"

# Encryption Key
PULSE_ENCRYPTION_KEY="your_encryption_key"
```

### Optional Variables

```bash
# Application environment (e.g., "production", "acceptance", "testing")
APP_ENV="acceptance"

# Base URL for the application dashboard (if applicable)
APP_BASE_URL="https://your-dashboard.com/"

# For testing purposes
PATIENT_ID="555555"
ORGANIZATION_ID="70"
```

## Usage

### Basic Authentication

```r
library(tfmGemsTracker)

# Get API information (requires environment variables)
api_info <- get_api_info(environment = "acceptance")

# Get access token
token <- get_access_token(api_info)
```

### Retrieving Patient Information

```r
# Get patient data
patient_info <- get_patient_info(
  api_info = api_info,
  token = token,
  patient_id = "123456"
)
```

### Querying Responses

```r
# Get questionnaire responses
responses <- get_responses(
  api_info = api_info,
  token = token,
  patient_id = "123456"
)
```

### Working with Careplans and Tasks

```r
# Get careplan information
careplan <- get_careplan_info(
  api_info = api_info,
  token = token,
  patient_id = "123456"
)

# Get task information
tasks <- get_task_info(
  api_info = api_info,
  token = token,
  task_id = "789"
)
```
