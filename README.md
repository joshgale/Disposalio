# Disposal.io

Disposal.io is a high-performance AFL Player Disposal Tracker. It uses Next.js 14 for the frontend and R (`fitzRoy`) for data processing, automated via GitHub Actions.

## Features
- **Real-time Filtering:** Search by player or filter by upcoming matches.
- **Dynamic History:** Toggle between last 5, 10, or 20 games to see evolving averages.
- **Trend Indicators:** Visual cues for players currently exceeding or falling below their seasonal averages.
- **Automated Data:** Data is refreshed daily at 4:00 AM AEST via GitHub Actions.

## Tech Stack
- **Frontend:** Next.js 14 (App Router), Tailwind CSS v4, Lucide React.
- **Data Engine:** R (`fitzRoy`, `dplyr`, `jsonlite`).
- **Automation:** GitHub Actions.
- **Deployment:** Vercel.

## Setup Instructions

### 1. GitHub Secrets
To allow the GitHub Action to push data updates back to your repository, you need to ensure the `GITHUB_TOKEN` has write permissions.

1. Go to your GitHub Repository **Settings**.
2. Navigate to **Actions** > **General**.
3. Under **Workflow permissions**, select **Read and write permissions**.
4. Click **Save**.

### 2. Local Development
To run the project locally:

```bash
# Install dependencies
npm install

# Run the development server
npm run dev
```

### 3. Data Processing (Optional)
If you have R installed and want to run the data fetcher manually:

```bash
Rscript scripts/get_afl_data.R
```

This will update `public/data/players.json`.

## Deployment
This project is structured for zero-config deployment on Vercel. 

1. Push your code to GitHub.
2. Connect your repository to Vercel.
3. Vercel will automatically detect the Next.js project and deploy it.
