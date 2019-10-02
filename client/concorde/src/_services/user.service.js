import $ from 'jquery';

export const userService = {
    login,
    logout,
    postRequest,
    getRequest
};

const url = 'http://localhost:8080/';

function login(username, password) {
    const requestOptions = {
        method: 'POST',
    };

    return fetch(url + 'login?user=' + username + '&password=' + password, requestOptions)
        .then(handleResponse)
        .then(data => {
            if (data.id) {
                localStorage.setItem('id', data.id);
                localStorage.setItem('user', data.user);
                localStorage.setItem('base-url', url);
            }
            return data.id;
        });
}

function logout() {
    localStorage.removeItem('id');
}

function sendRequest(serviceName, requestArgs, fetchArgs) {
    requestArgs = requestArgs || {};
    requestArgs.id = localStorage.getItem('id');
    fetchArgs = fetchArgs || '';

    return fetch('http://localhost:8080/' + serviceName + '?' + $.param(requestArgs), fetchArgs)
}
function postRequest(serviceName, args) {
    return sendRequest(serviceName, args, { method: "POST" });
}

function getRequest(serviceName, args) {
    return sendRequest(serviceName, args, { method: "GET" });
}

function handleResponse(response) {
    return response.text().then(text => {
        const data = text && JSON.parse(text);
        if (!response.ok) {
            if (response.status === 401) {
                // auto logout if 401 response returned from api
                logout();
                document.location.reload(true);
            }

            const error = (data && data.message) || response.statusText;
            return Promise.reject(error);
        }

        return data;
    });
}