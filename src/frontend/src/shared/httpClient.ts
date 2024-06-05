import axios from 'axios'
import { authToken } from './auth'

export const httpClient = axios.create({
  baseURL: process.env.REACT_APP_BACKEND_PATH,
  headers: {
    Authorization: `Bearer ${authToken.restore()}`,
  },
})
