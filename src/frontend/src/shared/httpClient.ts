import axios from 'axios'
import { authToken } from './auth'

export const httpClient = axios.create({
  baseURL: '/api',
  headers: {
    Authorization: `Bearer ${authToken.restore()}`,
  },
})
