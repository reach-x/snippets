export default {
	async fetch (request) {
		// Use the cf object to obtain the country of the request
		// more on the cf object: https://developers.cloudflare.com/workers/runtime-apis/request#incomingrequestcfproperties
		const country = request.cf.country;

		// If country is not null and is defined in the country map above, redirect.
		if (country != null && country != 'US') {

			return Response.redirect('https://www.paid-clinicals.com/non-us.html');
		} else {
			return fetch(request);
		}
	}
};

export default {
	async fetch(request) {
		const response = await fetch(request);

		// Clone the response so that it is no longer immutable
		const newResponse = new Response(response.body, response);

		if (request.cf.botManagement.score < 30) {
			const honeypot = "https://example.com/";
			return await fetch(honeypot, request);
		} else {
			return newResponse;
		}
	},
};