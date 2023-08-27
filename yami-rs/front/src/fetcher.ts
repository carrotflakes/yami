export const fetcher = (url: string, data: any) => fetch(url, {method: 'POST', body: data}).then(async (res) => [res.status, await res.text()])
